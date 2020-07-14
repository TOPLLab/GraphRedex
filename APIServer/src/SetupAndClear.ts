import arangojs, { Database } from "arangojs";
import * as os from "os";
import * as path from "path";
import * as readline from "readline";
import * as needle from "needle";
import MyDatabase from "./Database";
import { isReadableFile } from "./Utils";

//import arangojs, { Database, DocumentCollection, Graph } from "arangojs";

function logLn(text: string) {
    const minWidth = 50;
    if (text.length < minWidth - 8) {
        text += " ".repeat(minWidth - 8 - text.length);
    }
    console.log("┏" + "━".repeat(text.length + 2) + "┓");
    console.log(`┃ ${text} ┃`);
    console.log("┗" + "━".repeat(text.length + 2) + "┛");
}

export async function prepare(): Promise<void> {
    return await prepareReal().catch(async (e) => {
        console.log("\nSomething went wrong, checking db\n");
        await dbCheck(e); // check db
        console.log("\nTry again with new data\n");
        await prepareReal(); // and try again
        console.log("Success after retry");
        return;
    });
}

async function prepareReal(): Promise<void> {
    const db = await MyDatabase.bootstrap();

    logLn(`Removing all documents form DB...`);
    await db.rw.truncate().catch(async (e) => await dbCheck(e));
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
        await usersCollection.save({
            _key: "1",
            name: "demo",
            password: passwd,
        });

        console.log(" - Demo user added (_key = 1, demo:????)");

        if (isDemo) {
            console.warn(" WARNING: password for demo is demo!");
            console.warn(" DO NOT USE IN PRODUCTION");
            console.log(" To set demo password, place a hash in:");
        } else {
            console.log(" Password taken from:");
        }
        console.log(" " + path.join(os.homedir(), "graphredex-login.json"));
    }

    logLn("Examples graph");
    if (await examplesGraph.exists()) {
        console.log(" - Deleting existing graph");
        await examplesGraph.drop();
    }

    console.log(" - Creating: Examples per user graph created");
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

    console.log("done");
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

async function dbCheck(e: any): Promise<void> {
    if (!("isArangoError" in e && e.isArangoError)) {
        throw e;
    }

    switch (e.code) {
        case 401: // Unauthorized
            console.error("Could not login as graphredex");
            const api = await getRootDbAPIConnection();

            const dbName = "graphredex-data";
            const graphredexUsers = [
                { name: "graphredex", access: "rw" },
                { name: "graphredex-qry", access: "ro" },
            ];

            console.log("Getting users");
            const users: Map<string, any> = new Map(
                (await api("/_db/_system/_api/user/")).map((u) => [u.user, u]),
            );

            for (const { name: userName } of graphredexUsers) {
                console.log(`Checking ${userName}`);
                // Check if graphredex user exists
                if (users.has(userName)) {
                    // graphredex user exists
                    await api(`/_api/user/${userName}`, {
                        method: "PATCH",
                        body: {
                            active: true,
                            passwd: userName,
                        },
                    });
                    console.log(`  - activated and password is reset`);
                } else {
                    // create the user
                    await api("/_api/user", {
                        method: "POST",
                        body: {
                            user: userName,
                            active: true,
                            passwd: userName,
                        },
                    });
                    console.log(`  - created`);
                }
            }

            const databases: string[] = await api("/_api/database/");
            if (!databases.includes(dbName)) {
                // database does not exist, create
                await api("/_api/database", {
                    method: "POST",
                    body: {
                        name: dbName,
                        users: graphredexUsers.map((u) => ({
                            username: u.name,
                        })),
                    },
                });
            }

            console.log("Ensure proper permissions");
            for (const user of graphredexUsers) {
                console.log(`  - DB access for ${user.name}`);
                await api(`/_api/user/${user.name}/database/${dbName}`, {
                    method: "PUT",
                    body: { grant: user.access },
                });

                console.log(`  - collection access for ${user.name}`);
                await api(
                    `/_db/_system/_api/user/${user.name}/database/${dbName}/*`,
                    {
                        method: "PUT",
                        body: { grant: user.access },
                    },
                );
            }

            return;
            break;

        default:
            console.error("Could not fix error");
            throw e;
    }
}

async function getRootDbAPIConnection(): Promise<
    (path: string, options?: { method: string; body?: any }) => Promise<any>
> {
    const db: Database = arangojs({});
    const token = await db.login("root", await getRootPass());

    return (path: string, options: any = {}) =>
        new Promise((resolve, reject) => {
            needle(
                "get",
                "http://localhost:8529" + path,
                options.body,
                {
                    headers: {
                        Authorization: "Bearer " + token,
                    },
                    json: true,
                },
                (_err, _res, body) => {
                    if (!body.error) {
                        resolve(body.result);
                    } else {
                        reject(body);
                    }
                },
            );
        });
}

function getRootPass(): Promise<string> {
    return new Promise((resolve) => {
        if ("ARANGO_ROOT_PASSWORD" in process.env) {
            resolve(process.env.ARANGO_ROOT_PASSWORD);
        } else {
            console.log("Type root password:");
            const passInput = readline.createInterface({
                input: process.stdin,
                output: null,
                terminal: true,
            });

            passInput.question("Root pass", (pass) => {
                passInput.close();
                resolve(pass);
            });
        }
    });
}
