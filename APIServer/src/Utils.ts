import * as fs from "fs";

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
