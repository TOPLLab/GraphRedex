import { spawn } from "child_process";
import * as fs from "fs";
import { promisify } from "util";
import express = require("express");

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
                console.log(`rm exited with code ${code}`);
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

export function dirListing(path: string): Promise<fs.Dirent[]> {
    return new Promise((resolve, reject) => {
        fs.readdir(path, { withFileTypes: true }, (err, files: fs.Dirent[]) => {
            if (err) {
                reject(err);
            } else {
                resolve(files);
            }
        });
    });
}

export async function unzip(location: string, absDir: string) {
    // TODO: insecure !!! move to utils
    // unzip  -p example.zip | od | head -n 1001 | wc -l may help
    // echo $(( $(unzip  --aa -p zipbomb.zip | od -v | head -n 1001 | wc -l) == 1001))
    await new Promise((resolve, reject) => {
        const child = spawn(
            "unzip",
            [
                "-aa", // treat ALL files as text
                "-n", // never overwrite existing files
                location, // zip-file
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
    console.log("Unzipped " + location);
    await promisify(fs.unlink)(location);
}

export function asyncMiddleware(fn: express.RequestHandler) {
    return (req: express.Request, res: express.Response, next) => {
        Promise.resolve(fn(req, res, next)).catch(next);
    };
}

export function makeEnv(...overwrites: Object[]) {
    const filteredEnv = Object.keys(process.env)
        .filter(
            (k /* Keep useful vars */) =>
                [
                    "PATH",
                    "ARANGO_SERVER",
                    "ARANGO_PORT",
                    "DISPLAY",
                    "XAUTHORITY",
                ].includes(k) || k.startsWith("GRAPHREDEX_"),
        )
        .reduce((res, key) => ((res[key] = process.env[key]), res), {});
    return Object.assign({}, filteredEnv, ...overwrites);
}
