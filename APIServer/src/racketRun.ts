import { Graph } from "arangojs";
import { spawn } from "child_process";
import * as path from "path";
import { Readable } from "stream";
import MyDatabase from "./Database";
import Example, { ExampleMeta } from "./Example";
import { User } from "./Users";
import { isReadableFile, makeEnv } from "./Utils";

interface RacketResult {
    term: string | null;
    errors: string[];
}

interface IOResult {
    output: string;
    errors: string[];
    exitcode: number;
}

export default class ReductionRunner {
    private db: MyDatabase;
    private datadir: string;
    private runPath: string;
    /**
     *
     * @param db         Database connection for creating graphs
     * @param datadir    Directory in which the data files live
     */
    constructor(db: MyDatabase, datadir: string, runPath: string) {
        this.db = db;
        this.datadir = datadir;
        this.runPath = runPath;
    }

    public async run(
        saveName: string,
        user: User,
        lang: Language,
        term: string,
    ): Promise<ExampleMeta> {
        const graph = await this.db.reductionGraph(user, lang, true);
        const { term: termId } = await this.performReductions(
            term,
            graph,
            lang,
        ).catch((e) => {
            throw e;
        });

        const examplesCollection = this.db.examples(true);
        const usersExamplesCollection = this.db.rw.edgeCollection(
            "users-examples",
        );
        const example: ExampleMeta = (
            await examplesCollection.save(
                {
                    name: saveName,
                    baseTerm: termId,
                    baseTermString: term,
                    lang: lang._key,
                },
                { returnNew: true },
            )
        ).new;
        await usersExamplesCollection.save(
            { creator: user._id },
            user,
            example,
        );
        return example;
    }

    public async continue(
        user: User,
        example: Example,
        termKey: string,
    ): Promise<boolean> {
        const lang: Language = await example.getLanguage();
        const graph = await this.db.reductionGraph(user, lang, true);

        console.log("Starting form", termKey);
        const term: TermMeta = (
            await graph.vertexCollection(graph.name).lookupByKeys([termKey])
        )[0];

        await this.startRun(
            [
                lang.debugging ? "DebuggerServer" : "EchoServer",
                "run-echo",
                path.join(this.datadir, lang.path),
                graph.name,
                "50",
            ],
            term.term,
        ).then(({ output, errors }) => ({ term: output, errors: errors }));

        return true;
    }

    private async performReductions(
        term: string,
        graph: Graph,
        lang: Language,
    ): Promise<RacketResult> {
        if (!(await isReadableFile(path.join(this.datadir, lang.path)))) {
            throw lang.path + " is not found";
        }

        return await this.startRun(
            [
                lang.debugging ? "DebuggerServer" : "EchoServer",
                "run-echo",
                path.join(this.datadir, lang.path),
                graph.name,
                "50",
            ],
            term,
        ).then(({ output, errors }) => ({
            term: output.split(/\n|\r/).pop(),
            errors: errors,
        }));
    }

    private async startRun(
        params: string[],
        stdin: string = null,
    ): Promise<IOResult> {
        return new Promise<IOResult>((resolve, reject) => {
            const env = makeEnv({ LC_ALL: "C" });
            console.log(env);
            const child = spawn(this.runPath, params, {
                env: env,
                stdio: ["pipe", "pipe", "pipe"],
            });

            if (stdin) {
                console.log("supplying", stdin);
                const stdinStream = new Readable();
                stdinStream.on("error", (e) => {
                    reject(e);
                });
                if (stdinStream.push(stdin)) {
                    // Add data to the internal queue for users of the stream to consume
                    stdinStream.push(null); // Signals the end of the stream (EOF)
                }
                stdinStream.pipe(child.stdin);
                console.log("Term supplied");
            }

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
                console.log("Now ", process.cwd() + "/", err);
                errors.push(err.message);
                reject(errors);
                child.kill();
            });

            child.on("exit", (code) => {
                console.log(`child process [exited] with code ${code}`);
                if (code === 0) {
                    resolve({ output: output, errors: errors, exitcode: code });
                } else {
                    reject(errors);
                }
            });

            child.on("close", (code) => {
                console.log(`child process exited with code ${code}`);
                if (code === 0) {
                    resolve({ output: output, errors: errors, exitcode: code });
                } else {
                    reject(errors);
                }
            });
        });
    }
}
