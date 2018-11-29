import * as fs from "fs";
import express = require("express");
import { spawn } from "child_process";

export function isReadableFile(absPath: string): Promise<boolean> {
    return new Promise((resolve) => {
        fs.access(absPath, fs.constants.R_OK, (err) => {
            resolve(err ? false : true);
        });
    });
}

export function deleteDir(
    absPath: string,
    recursive: boolean = false,
): Promise<boolean> {
    if (recursive) {
        return new Promise((resolve, reject) => {
            const args = [
                "-r", // treat ALL files as text
                "--", // never overwrite existing files
                absPath, // path
            ];
            console.log("rm", args);
            const child = spawn("rm", args, {
                env: { LC_ALL: "C" },
                stdio: ["pipe", "pipe", "pipe"],
            });

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
    } else {
        return new Promise((resolve, reject) => {
            fs.rmdir(absPath, (err) => {
                if (err) {
                    reject(err);
                } else {
                    resolve(false);
                }
            });
        });
    }
}

export function asyncMiddleware(fn: express.RequestHandler) {
    return (req: express.Request, res: express.Response, next) => {
        Promise.resolve(fn(req, res, next)).catch(next);
    };
}
