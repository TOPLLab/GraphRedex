import { spawn } from "child_process";
import { Readable } from "stream";
import * as path from "path";
import MyDatabase from "./Database";
import { User } from "./Users";
import { ExampleMeta } from "./Example";

interface RacketResult {
    term: string | null;
    errors: string[];
}

export default class ReductionRunner {
    private db: MyDatabase;
    private datadir: string;
    /**
     *
     * @param db         Database connection for creating graphs
     * @param datadir    Directory in which the datafiles live
     */
    constructor(db: MyDatabase, datadir: string) {
        this.db = db;
        this.datadir = datadir;
    }

    public async run(
        saveName: string,
        user: User,
        lang: Language,
        term: string,
    ): Promise<ExampleMeta> {
        await this.db.reductionGraph(user, lang, true);
        const { term: termId } = await this.performReductions(
            term,
            user,
            lang,
        ).catch((e) => {
            throw e;
        });

        const examplesCollection = this.db.examples(true);
        const usersExamplesCollection = this.db.rw.edgeCollection(
            "users-examples",
        );
        const example = await examplesCollection.save({
            name: saveName,
            baseTerm: termId,
            lang: +lang,
        });
        await usersExamplesCollection.save(
            { creator: user._id },
            user,
            example,
        );
        return example;
    }

    private performReductions(
        term: string,
        user: User,
        lang: Language,
    ): Promise<RacketResult> {
        const gname = `test-${user}-${lang}`;

        return new Promise<RacketResult>((resolve, reject) => {
            const child = spawn(
                "RedexServer/run.sh",
                [path.join(this.datadir, lang.path), /* Graph name */ gname],
                {
                    cwd:
                        "/home/beardhatcode/Documents/doctoraat/RelatedWork/GraphRedex/code/",
                    env: { LC_ALL: "C" },
                    stdio: ["pipe", "pipe", "pipe"],
                },
            );

            console.log("supplying", term);
            const stdinStream = new Readable();
            stdinStream.push(term); // Add data to the internal queue for users of the stream to consume
            stdinStream.push(null); // Signals the end of the stream (EOF)
            stdinStream.pipe(child.stdin);
            console.log("Term supplied");

            let output: string = "";
            child.stdout.on("data", (data) => {
                console.log(`stdout: ${data}`);
                output += data.toString();
            });

            const errors: string[] = [];
            child.stderr.on("data", (data) => {
                console.log(`err: ${data}`);
                if (errors.length < 5) {
                    errors.push(data);
                }
            });

            child.on("error", (err) => {
                reject(err);
            });

            child.on("close", (code) => {
                console.log(`child process exited with code ${code}`);
                if (code === 0) {
                    resolve({ term: output, errors: errors });
                } else {
                    reject(errors);
                }
            });
        });
    }
}