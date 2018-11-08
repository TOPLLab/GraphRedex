import MyDatabase from "./Database";
import * as fs from "fs";
import * as path from "path";
import { User } from "./Users";
import { spawn } from "child_process";
const { promisify } = require("util");

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
    ): Promise<{ lang: Language; absPath: string; absDir: string }> {
        const langData: Language = {
            name: name,
            path: "",
            dir: "",
            onDisk: false,
        };
        const metaData = await this.db.languages(true).save(langData);
        langData._key = metaData._key;

        langData.dir = path.join(user._key, "lang", metaData._key);
        langData.path = path.join(langData.dir, "PLTGraphRedex.rkt");

        const absPathDir = path.join(this.datadir, langData.dir);
        const absPath = path.join(this.datadir, langData.path);

        await this.db.rw
            .edgeCollection("users-languages")
            .save({ creator: true }, user, {
                _id: "languages/" + langData._key,
            });
        // TODO: use id instead of key

        await promisify(fs.mkdir)(absPathDir);
        return { lang: langData, absPath: absPath, absDir: absPathDir };
    }

    private async confirmDisk(lang: Language): Promise<Language> {
        // Check if main file is readable
        await new Promise((resolve, reject) => {
            const absPath = path.join(this.datadir, lang.path);
            fs.access(absPath, fs.constants.R_OK, (err) => {
                if (err) {
                    fs.rmdir(absPath, () => {
                        reject("File not readable, no " + lang.path + " found");
                    });
                } else {
                    resolve();
                }
            });
        });

        lang.onDisk = true;
        await this.db.languages(true).replace({ _key: "" + lang._key }, lang);

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
    ): Promise<Language> {
        const { lang, absPath } = await this.createInDB(user, name);

        await promisify(fs.rename)(location, absPath);

        lang.onDisk = true;

        await this.db.languages(true).replace({ _key: "" + lang._key }, lang);

        return this.confirmDisk(lang);
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
    ): Promise<Language> {
        const { lang, absDir } = await this.createInDB(user, name);

        if (!absDir.startsWith(this.datadir)) {
            throw "Invalid filename";
        }

        await promisify(fs.mkdir)(absDir);

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
        await promisify(fs.unlink)(location); //delete zip

        return await this.confirmDisk(lang);
    }
}
