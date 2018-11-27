import * as fs from "fs";
import express = require("express");

export function isReadableFile(absPath: string): Promise<boolean> {
    return new Promise((resolve) => {
        fs.access(absPath, fs.constants.R_OK, (err) => {
            resolve(err ? false : true);
        });
    });
}

export function deleteDir(absPath: string): Promise<boolean> {
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

export function asyncMiddleware(fn: express.RequestHandler) {
    return (req: express.Request, res: express.Response, next) => {
        Promise.resolve(fn(req, res, next)).catch(next);
    };
}
