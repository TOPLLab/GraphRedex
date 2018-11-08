import { pbkdf2, randomBytes } from "crypto";

export interface HashedPass {
    salt: string;
    hash: string;
    iterations: number;
}

export function hashPassword(password: string): Promise<HashedPass> {
    const salt = randomBytes(5).toString("base64");
    const iterations = 10000;

    return new Promise<HashedPass>((resolve, reject) => {
        pbkdf2(password, salt, iterations, 512, "sha512", (err, hash) => {
            if (err) {
                reject(err);
            }
            resolve({
                salt: salt,
                hash: hash.toString("base64"),
                iterations: iterations,
            });
        });
    });
}

export function isPasswordCorrect(
    saved: HashedPass,
    passwordAttempt: string,
): Promise<boolean> {
    return new Promise<boolean>((resolve, reject) => {
        pbkdf2(
            passwordAttempt,
            saved.salt,
            saved.iterations,
            512,
            "sha512",
            (err, hash) => {
                if (err) {
                    reject(err);
                }
                resolve(hash.toString("base64") === saved.hash);
            },
        );
    });
}
