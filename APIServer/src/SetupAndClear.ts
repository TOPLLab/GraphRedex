import MyDatabase from "./Database";
import * as os from "os";
import * as path from "path";
import { isReadableFile } from "./Utils";

function logLn(text: string) {
    const minWidth = 50;
    if (text.length < minWidth - 8) {
        text += " ".repeat(minWidth - 8 - text.length);
    }
    console.log("┏" + "━".repeat(text.length + 2) + "┓");
    console.log(`┃ ${text} ┃`);
    console.log("┗" + "━".repeat(text.length + 2) + "┛");
}

export async function prepare() {
    const db = await MyDatabase.bootstrap();

    logLn(`Removing all documents form DB...`);
    await db.rw.truncate();
    console.log("done");

    logLn(`Renewing collections and graph...`);

    console.log("Deleting all collections");
    await Promise.all(
        await db.rw.collections().then((cs) =>
            cs.map(async (c) => {
                // tslint:disable-next-line
                await c["drop"]();
                console.log(` - ${c.name} dropped`);
            }),
        ),
    );

    console.log("Deleting all Graphs");
    await Promise.all(
        await db.rw.graphs().then((cs) =>
            cs.map(async (c) => {
                // tslint:disable-next-line
                await c["drop"]();
                console.log(` - ${c.name} dropped`);
            }),
        ),
    );

    console.log("Creating collections");
    const languagesCollection = db.rw.collection("languages");
    const usersLanguagesCollection = db.rw.edgeCollection("users-languages");
    const usersCollection = db.rw.collection("users");
    const examplesCollection = db.rw.collection("examples");
    const usersExamplesCollection = db.rw.edgeCollection("users-examples");
    const examplesGraph = db.rw.graph("Data");

    await Promise.all(
        [
            languagesCollection,
            usersLanguagesCollection,
            usersCollection,
            examplesCollection,
            usersExamplesCollection,
        ].map(async (c) => {
            await c.create().catch((e) => {
                console.log(` ! ${c.name} could not be created`);
                throw e;
            });
            console.log(` - ${c.name} collection created`);
        }),
    );

    if (!(await usersCollection.documentExists("1"))) {
        logLn("Create demo user");
        const [isDemo, passwd] = await getPass();
        console.log(passwd);
        await usersCollection.save({
            _key: "1",
            name: "demo",
            password: passwd,
        });
        console.log("Demo user added (_key = 1, demo:????)");

        if (isDemo) {
            console.warn("WARNING: password for demo is demo!");
            console.warn("DO NOT USE IN PRODUCTION");
        }
    }

    logLn("renew examples graph");
    if (await examplesGraph.exists()) {
        console.log("Deleting");
        await examplesGraph.drop();
    }

    console.log("Creating");
    await examplesGraph.create({
        edgeDefinitions: [
            {
                collection: usersExamplesCollection.name,
                from: [usersCollection.name],
                to: [examplesCollection.name],
            },
            {
                collection: usersLanguagesCollection.name,
                from: [usersCollection.name],
                to: [languagesCollection.name],
            },
        ],
    });

    console.log("Examples per user graph created");
    return;
}

async function getPass(): Promise<[boolean, Object]> {
    const passwdPath = path.join(os.homedir(), "graphredex-login.json");
    if (await isReadableFile(passwdPath)) {
        return [false, await require(passwdPath)];
    } else {
        return [
            true,
            {
                // default pass: "demo"
                salt: "B0+PC18=",
                hash:
                    // tslint:disable-next-line:max-line-length
                    "F9LRnLwgcaqW1IBDEhtNgSX6gLbX6Ye1AtTAnMmal0CPERVs1odDLxX85F02dpg05/CyS6RiPsirMF8FnH+PX3uU7NIznx7eAzNoWFV57zuEgqbEevnJbp5eDzbji5TceCMesZ/Mz0AeGc9LV/jz4OiEhEKLRGEpmKCi0J1YJSJTSLBMd62P1UsCV1Mv5UOZYrwNAqnzvpfqN17Qz4x8ZWQsaJdrEpMiG/Bav4z7LL+xtmbPeEEOh2W0Gk01cuSc+U2la0rwV81uh16lKXp1nUyOo9qY3G+IbR1CnFFrcnud3kSipqjtzrH/UiDBFzyTV61MbZvx8c/BubrIL7LaafLA0VUsNo0JNDpmnu6EQtO4+0mqBPUG5njLo+hsaMPSKfIvXTQZq5nAsC1QmXH69UB+tW82JPxyEa4U5fKADJKl5QhnjoBzgXzvhhVSGIsu3hRIi0a4ZG7GxGUUKEqU6TcONFda1+W+tRpnFqylD3ogT7ikvjWDJeG8AaDeRmx+dCn3Fmp49yqDAAp25D+DuVClcCL4Uns69ge4tKDaQ9W8yhtHjjaPXFc45KU+sYcp7DkdLvKTUcIoF/aClELYOp2anOhMtDbONUS/xuSZCqRYcIRDSSUFobes2cm0ggXtTlpmc2yjO6/RjqaiebkAA+YloabmSr//nGaIuDmXd5U=",
                iterations: 10000,
            },
        ];
    }
}

prepare().then(console.log);
