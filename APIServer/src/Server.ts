"use strict";

import * as auth from "basic-auth";
import * as bodyParser from "body-parser";
import * as express from "express";
import * as multer from "multer";
import MyDatabase from "./Database";
import { Languages } from "./Languages";
import ReductionRunner from "./racketRun";
import Users, { User } from "./Users";
import { asyncMiddleware } from "./Utils";
import Example from "./Example";

/**
 * The server.
 *
 * @class Server
 */
export default class Server {
    public app: express.Application;
    private database: MyDatabase;
    private users: Users;
    private runner: ReductionRunner;
    private uploader;
    private languages: Languages;

    /**
     * Bootstrap the application.
     */
    public static async bootstrap(
        dataDir: string,
        runPath: string,
    ): Promise<Server> {
        return new Server(await MyDatabase.bootstrap(), dataDir, runPath);
    }

    /**
     * Constructor.
     *
     * @class Server
     * @constructor
     */
    private constructor(
        database: MyDatabase,
        datadir: string,
        runPath: string,
    ) {
        this.database = database;
        this.users = new Users(database);
        this.runner = new ReductionRunner(this.database, datadir, runPath);
        this.languages = new Languages(database, datadir);
        this.uploader = multer({ dest: datadir + "/tmp/" });

        //create expressjs application
        this.app = express();

        //configure application
        this.routes();
    }

    private routes() {
        const routeMy = express.Router();
        this.app.use("/my", routeMy);

        routeMy.post(
            "/example/qry/:id([0-9,]+)",
            bodyParser.text({ type: "*/*" }),
            this.requireLogin(
                async (
                    user: User,
                    req: express.Request,
                    res: express.Response,
                ) => {
                    const qry = req.body.toString();
                    const example: Example = await this.users.exampleOf(user, {
                        _key: req.params.id,
                    });
                    res.jsonp(await example.qry(qry));
                },
            ),
        );

        routeMy.get(
            "/examples",
            this.requireLogin(
                async (
                    user: User,
                    _req: express.Request,
                    res: express.Response,
                ) => {
                    res.jsonp(await this.users.exmplesOf(user));
                },
            ),
        );

        routeMy.get(
            "/languages",
            this.requireLogin(
                async (
                    user: User,
                    _req: express.Request,
                    res: express.Response,
                ) => {
                    res.jsonp(await this.users.languagesOf(user));
                },
            ),
        );

        routeMy.post(
            "/languages",
            this.uploader.single("specification"),
            this.requireLogin(
                async (
                    user: User,
                    req: express.Request,
                    res: express.Response,
                ) => {
                    const file: MulterDiskFile = req["file"]; // tslint:disable-line
                    const filenameParts = file.originalname.split(".");
                    const extension: string = filenameParts.pop();
                    const fileName: string = filenameParts.join(".");
                    switch (extension) {
                        case "zip":
                            await this.languages.createFormZip(
                                user,
                                fileName,
                                file.path,
                            );
                            res.jsonp({ ok: true, type: "zip", r: fileName });
                            break;
                        case "rkt":
                            await this.languages.createFormSingleFile(
                                user,
                                fileName,
                                file.path,
                            );
                            res.jsonp({
                                ok: true,
                                type: "single",
                                r: fileName,
                            });

                            break;
                        default:
                            res.status(500).jsonp({
                                ok: false,
                                note: "only submit zip and or rkt files please",
                            });
                    }
                },
            ),
        );

        this.app.post(
            "/doTerm/:lang/:name",
            bodyParser.text({ type: "*/*" }),
            this.requireLogin(
                async (
                    user: User,
                    req: express.Request,
                    res: express.Response,
                ) => {
                    const term = req.body.toString();
                    const lang: Language = await this.users.languageOf(user, {
                        _key: req.params.lang,
                    });

                    await this.runner
                        .run(req.params.name, user, lang, term)
                        .then((example) => {
                            res.status(201).jsonp({
                                lang: lang,
                                term: term,
                                output: example.baseTerm,
                                example: example,
                            });
                        })
                        .catch((e) => {
                            res.status(500).jsonp({
                                lang: lang,
                                term: term,
                                output: null,
                                e: "somthing went wrong",
                                errors: e.toString(),
                            });
                        });
                },
            ),
        );

        this.app.post(
            "/continueTerm/:example/:termKey",
            this.requireLogin(
                async (
                    user: User,
                    req: express.Request,
                    res: express.Response,
                ) => {
                    const example: Example = await this.users.exampleOf(user, {
                        _key: req.params.example,
                    });
                    console.log(req.params.termKey);
                    await this.runner
                        .continue(user, example, req.params.termKey)
                        .then((example) => {
                            res.status(201).jsonp({
                                term: req.params.termKey,
                                output: true,
                                example: example,
                            });
                        })
                        .catch((e) => {
                            res.status(500).jsonp({
                                term: req.params.termKey,
                                output: null,
                                e: "somthing went wrong",
                                errors: e.toString(),
                            });
                        });
                },
            ),
        );
    }

    /* Helper functions allowing async */

    private requireLogin(
        fn: (
            user: User,
            req: express.Request,
            res: express.Response,
            next: express.NextFunction,
        ) => Promise<any>,
    ) {
        return asyncMiddleware(
            async (
                req: express.Request,
                res: express.Response,
                next: express.NextFunction,
            ) => {
                const loginData = auth(req);
                if (loginData && "name" in loginData && "pass" in loginData) {
                    const user: User = await this.users
                        .byAuth(loginData)
                        .catch((e) => {
                            res.set(
                                "WWW-Authenticate",
                                `Basic realm="Graph redex (${e})"`,
                            ); // change this
                            res.status(401).send("Authentication required."); // custom message
                            return null;
                        });
                    if (user) {
                        return await fn(user, req, res, next);
                    }
                } else {
                    res.set("WWW-Authenticate", 'Basic realm="Graph redex"'); // change this
                    res.status(401).send("Authentication required."); // custom message
                }
                return;
            },
        );
    }
}
