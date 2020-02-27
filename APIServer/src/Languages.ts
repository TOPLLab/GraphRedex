import * as fs from "fs";
import * as path from "path";
import MyDatabase from "./Database";
import { User } from "./Users";
import { deleteDir, dirListing, isReadableFile, unzip } from "./Utils";
const { promisify } = require("util");

const BASE_FILENAME = "PLTGraphRedex.rkt";

export class Languages {
    private db: MyDatabase;
    private datadir: string;

    constructor(db: MyDatabase, datadir: string) {
        this.db = db;
        this.datadir = datadir;
    }

    async addQry(lang: Language, name: string, query: string) {
        if (name.length < 1) {
            throw "Cannot store zero length name";
        }
        if (name.length < 3) {
            throw "Query too short";
        }
        if (!("query" in lang)) {
            lang.query = [];
        }
        lang.query.push({ name, query });
        await this.db.languages(true).replace("" + lang._key, lang);
    }

    async delQry(lang: Language, name: string) {
        if ("query" in lang) {
            lang.query = lang.query.filter((x) => x.name !== name);

            await this.db.languages(true).replace("" + lang._key, lang);
        }
    }

    private async createInDB(
        user: User,
        name: string,
    ): Promise<{ lang: Language; absPath: string; absDir: string }> {
        const langData: Language = {
            name: name,
            path: "",
            dir: "",
            onDisk: false,
        };
        const [metaData] = await this.db.languages(true).save([langData]);
        langData._key = metaData._key;

        langData.dir = path.join(user._key, "lang", metaData._key);
        langData.path = path.join(langData.dir, BASE_FILENAME);

        const absPathDir = path.join(this.datadir, langData.dir);
        const absPath = path.join(this.datadir, langData.path);

        if (
            !absPathDir.startsWith(this.datadir) ||
            !absPath.startsWith(this.datadir)
        ) {
            throw "Invalid filename";
        }

        // TODO: use id instead of key

        await promisify(fs.mkdir)(absPathDir, { recursive: true });
        return { lang: langData, absPath: absPath, absDir: absPathDir };
    }

    /**
     * Check if a language is sucessfuly placed on disk
     *
     * If so flag it in the database and add the user as owner
     */
    private async confirmDisk(lang: Language, user: User): Promise<Language> {
        const absBaseFileName = path.join(this.datadir, lang.path);
        if (
            !absBaseFileName.startsWith(this.datadir) || // Path outside dir
            !(await isReadableFile(absBaseFileName)) // Not readable
        ) {
            await deleteDir(path.join(this.datadir, lang.dir), true).catch(
                console.error,
            );
            await this.db
                .languages(true)
                .remove({ _key: "" + lang._key })
                .catch(console.error);
            throw lang.path + " Could not be created";
        }

        lang.onDisk = true;
        await this.db.languages(true).replace({ _key: "" + lang._key }, lang);

        await this.db.rw
            .edgeCollection("users-languages")
            .save({ creator: true }, user, { _id: "languages/" + lang._key });

        return lang;
    }
    /**
     *
     * @param user User this language belongs to
     * @param name Name of language
     * @param location of the stored tmp file (will be moved)
     */
    async createFormSingleFile(
        user: User,
        name: string,
        location: string,
    ): Promise<Language> {
        const { lang, absPath } = await this.createInDB(user, name);

        await promisify(fs.rename)(location, absPath);

        lang.onDisk = true;

        await this.db.languages(true).replace({ _key: "" + lang._key }, lang);

        return this.confirmDisk(lang, user);
    }

    /**
     *
     * @param user User this language belongs to
     * @param name Name of language
     * @param location of the stored tmp file (will be moved)
     */
    async createFormZip(
        user: User,
        name: string,
        location: string,
    ): Promise<Language> {
        const { lang, absDir } = await this.createInDB(user, name);

        if (!absDir.startsWith(this.datadir)) {
            throw "Invalid filename";
        }

        await unzip(location, absDir);

        // Move base dir one up if only one dir in zip
        const rootDirList = (await dirListing(absDir)).filter(
            (x) => !(x.name === "__MACOSX" && x.isDirectory()),
        );
        if (rootDirList.length === 1) {
            if (rootDirList[0].isDirectory()) {
                lang.dir = path.join(lang.dir, rootDirList[0].name);
                lang.path = path.join(lang.dir, BASE_FILENAME);
            }
        }

        return await this.confirmDisk(lang, user);
    }
}
