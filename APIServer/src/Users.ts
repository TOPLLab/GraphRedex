import MyDatabase from "./Database";
import { HashedPass, isPasswordCorrect } from "./password";
import { Graph, DocumentCollection } from "arangojs";
import Example from "./Example";

export interface User {
    _key: string;
    _id: string;
    name: string;
    password: HashedPass;
}

export default class Users {
    private database: MyDatabase;
    private datagraph: Graph;
    private usercollecion: DocumentCollection;

    constructor(database: MyDatabase) {
        this.database = database;
        this.datagraph = database.ro.graph("data");
        this.usercollecion = this.database.ro.collection("users");
    }

    public async byKey(key: string): Promise<User> {
        return await this.usercollecion.lookupByKeys([key])[0];
    }

    public async byAuth(auth: { name: string; pass: string }): Promise<User> {
        const dbdata = await this.usercollecion
            .firstExample({ name: auth.name })
            .catch(() => null);
        if (dbdata && (await isPasswordCorrect(dbdata.password, auth.pass))) {
            return dbdata;
        }
        console.log(
            `Failed login attempt for ${encodeURIComponent(auth.name)} (${
                dbdata === null ? "not found" : "found"
            })`,
        );
        throw "Invalid credentials or user not found. ";
    }

    public async exmplesOf(user: User | { _key: string }): Promise<Example[]> {
        const key: String = "users/" + user._key;
        const exIds: { _to: string }[] = await this.datagraph
            .edgeCollection("users-examples")
            .byExample({ _from: key })
            .then((c) => c.all());
        return await this.database
            .examples(false)
            .lookupByKeys(exIds.map((x) => x._to));
    }

    public async exampleOf(
        user: User | { _key: string },
        example: { _key: string },
    ): Promise<Example> {
        // TODO: move
        const userKey: string = "users/" + user._key;
        const exampleKey: string = "examples/" + example._key;
        const results: Number = await this.datagraph
            .edgeCollection("users-examples")
            .byExample({ _from: userKey, _to: exampleKey })
            .then((c) => c.count);
        if (results === 1) {
            const example: Example = new Example(
                this.database,
                (
                    await this.datagraph
                        .vertexCollection("examples")
                        .lookupByKeys([exampleKey])
                )[0],
            );
            return await example;
        }
        throw "Example not found or you do not have access to it";
    }

    public async languagesOf(user: User): Promise<Language[]> {
        const key: String = "users/" + user._key;
        const exIds: { _to: string }[] = await this.datagraph
            .edgeCollection("users-languages")
            .byExample({ _from: key })
            .then((c) => c.all());
        return await this.database
            .languages(false)
            .lookupByKeys(exIds.map((x) => x._to));
    }

    public async languageOf(
        user: User | { _key: string },
        language: { _key: string },
    ): Promise<Language> {
        // TODO: move
        const userKey: string = "users/" + user._key;
        const languageKey: string = "languages/" + language._key;
        const results: Number = await this.datagraph
            .edgeCollection("users-languages")
            .byExample({ _from: userKey, _to: languageKey })
            .then((c) => c.count);
        if (results === 1) {
            const langs = await this.database
                .languages(false)
                .lookupByKeys([languageKey]);
            if (langs.length !== 1) {
                throw "Internal error";
            }
            return langs[0];
        }
        throw "Language not found or you do not have access to it";
    }
}
