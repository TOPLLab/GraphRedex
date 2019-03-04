import arangojs, { Database, DocumentCollection, Graph } from "arangojs";
import { User } from "./Users";

export default class MyDatabase {
    private constructor(rwConnection: Database, roConnection: Database) {
        this.dbRO = roConnection;
        this.dbRW = rwConnection;
    }

    public static async bootstrap(): Promise<MyDatabase> {
        const dbRO = arangojs({});
        await dbRO.login("graphredex-qry", "graphredex-qry"); // read only acces
        dbRO.useDatabase("graphredex-data");

        const dbRW = arangojs({});
        await dbRW.login("graphredex", "graphredex"); // rw acces
        dbRW.useDatabase("graphredex-data");
        return new MyDatabase(dbRW, dbRO);
    }

    private dbRO: Database;
    private dbRW: Database;

    get ro() {
        return this.dbRO;
    }

    get rw() {
        return this.dbRW;
    }

    public users(write: boolean = false): DocumentCollection {
        return this.connection(write).collection("users");
    }

    public examples(write: boolean = false): DocumentCollection {
        return this.connection(write).collection("examples");
    }

    public languages(write: boolean = false): DocumentCollection {
        return this.connection(write).collection("languages");
    }

    private connection(write: boolean) {
        return write ? this.dbRW : this.dbRO;
    }

    public async reductionGraph(
        user: User,
        lang: Language,
        createIfNotExisit: boolean = false,
    ): Promise<Graph> {
        const name = `results-${user._key}-${lang._key}`;
        const graph = this.dbRW.graph(name);
        const nodeCollecion = this.dbRW.collection(name);
        const edgeCollection = this.dbRW.edgeCollection(name + "-reductions");

        if (!(await nodeCollecion.exists())) {
            if (!createIfNotExisit) {
                throw `Reduction graph ${name} does not exist`;
            }
            await nodeCollecion.create();
            await nodeCollecion.createIndex({
                type: "hash",
                fields: ["term"],
            });
        }

        if (!(await graph.exists())) {
            if (!createIfNotExisit) {
                throw `Reduction graph ${name} does not exist`;
            }
            await graph.create({
                edgeDefinitions: [
                    {
                        collection: edgeCollection.name,
                        from: [nodeCollecion.name],
                        to: [nodeCollecion.name],
                    },
                ],
            });
        }

        return graph;
    }
}
