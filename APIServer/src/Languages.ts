import MyDatabase from "./Database";
import * as fs from "fs";
import * as path from "path";
import { User } from "./Users";
import { spawn } from "child_process";
import { isReadableFile, deleteDir, dirListing } from "./Utils";
const { promisify } = require("util");

const BASE_FILENAME = "PLTGraphRedex.rkt";

export class Languages {
    private db: MyDatabase;
    private datadir: string;

    constructor(db: MyDatabase, datadir: string) {
        this.db = db;
        this.datadir = datadir;
    }

    private async createInDB(
        user: User,
        name: string,
        debugging: boolean,
    ): Promise<{ lang: Language; absPath: string; absDir: string }> {
        const langData: Language = {
            name: name,
            path: "",
            dir: "",
            onDisk: false,
            debugging: debugging,
        };
        const metaData = await this.db.languages(true).save(langData);
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
     * @param name Name of lannguage
     * @param location of the stored tmp file (will be moved)
     */
    async createFormSingleFile(
        user: User,
        name: string,
        location: string,
        debugging: boolean,
    ): Promise<Language> {
        const { lang, absPath } = await this.createInDB(user, name, debugging);

        await promisify(fs.rename)(location, absPath);

        lang.onDisk = true;

        await this.db.languages(true).replace({ _key: "" + lang._key }, lang);

        return this.confirmDisk(lang, user);
    }

    /**
     *
     * @param user User this language belongs to
     * @param name Name of lannguage
     * @param location of the stored tmp file (will be moved)
     */
    async createFormZip(
        user: User,
        name: string,
        location: string,
        debugging: boolean,
    ): Promise<Language> {
        const { lang, absDir } = await this.createInDB(user, name, debugging);

        if (!absDir.startsWith(this.datadir)) {
            throw "Invalid filename";
        }

        // TODO: insecure !!! move to utils
        // unzip  -p example.zip | od | head -n 1001 | wc -l may help
        // echo $(( $(unzip  --aa -p zipbomb.zip | od -v | head -n 1001 | wc -l) == 1001))
        await new Promise((resolve, reject) => {
            const child = spawn(
                "unzip",
                [
                    "-aa", // treat ALL files as text
                    "-n", // never overwrite existing files
                    location, // zipfile
                    "-d",
                    absDir, // destination
                ],
                {
                    env: { LC_ALL: "C" },
                    stdio: ["pipe", "pipe", "pipe"],
                },
            );

            child.stdout.on("data", (data) => {
                console.log(`stdout: ${data}`);
            });

            child.stderr.on("data", (data) => {
                console.log(`err: ${data}`);
            });

            child.on("error", reject);

            child.on("close", (code) => {
                console.log(`unzip exited with code ${code}`);
                if (code === 0) {
                    resolve(true);
                } else {
                    reject(false);
                }
            });
        });
        console.log("Unzipped " + location);
        await promisify(fs.unlink)(location);

        // Move base dir one up if only one dir in zip
        const files = await dirListing(absDir);
        if (files.length === 1) {
            if (files[0].isDirectory()) {
                lang.dir = path.join(lang.dir, files[0].name);
                lang.path = path.join(lang.dir, BASE_FILENAME);
            }
        }

        return await this.confirmDisk(lang, user);
    }
}
