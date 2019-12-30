"use strict";

import * as express from "express";
import MyDatabase from "./Database";
import Server from "./Server";
import { User } from "./Users";
import { asyncMiddleware } from "./Utils";

/**
 * The server logged in with demo:demo
 *
 * @class ServerNoLogin
 */
export default class ServerNoLogin extends Server {
    public static async bootstrap(
        dataDir: string,
        runPath: string,
    ): Promise<Server> {
        return new ServerNoLogin(
            await MyDatabase.bootstrap(),
            dataDir,
            runPath,
        );
    }

    protected requireLogin(
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
                const user: User = await this.users.byAuth({
                    name: "demo",
                    pass: "demo",
                });
                if (user) {
                    return await fn(user, req, res, next);
                }
                return;
            },
        );
    }
}
