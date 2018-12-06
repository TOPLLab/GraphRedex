/**
 * Wrapper around window.fetch that returns JSON
 * and throws on nonzero output
 */
export function getit(input: RequestInfo, init: RequestInit): Promise<any> {
    return new Promise((resolve, reject) => {
        window
            .fetch(input, init)
            .then((x) => {
                if (x.ok) {
                    x.text().then((txt) => {
                        console.log(txt);
                        try {
                            const data = JSON.parse(txt);
                            resolve(data);
                        } catch (e) {
                            reject(txt);
                        }
                    });
                } else {
                    x.text()
                        .then((txt) => {
                            try {
                                const data = JSON.parse(txt);
                                reject(data);
                            } catch (e) {
                                reject(txt);
                            }
                        })
                        .catch(reject);
                }
            })
            .catch((e) => reject(e));
    });
}

/**
 * Do a check
 * @param {*} getStatus gets the current status
 * @param {*} onChange function called on chenge of getStatus output
 * @param {*} interval interval for checking
 */
export function setCheckInterval(
    getStatus: () => any,
    onChange: (ststus: any) => any,
    interval: number,
) {
    let oldStatus = getStatus();
    window.setInterval(() => {
        const newStatus = getStatus();
        if (oldStatus !== newStatus) {
            onChange(newStatus);
        }
        oldStatus = newStatus;
    }, interval);
    onChange(oldStatus);
}

export function fileToText(file: File) {
    return new Promise((resolve) => {
        const reader = new FileReader();
        reader.onload = function() {
            const text = reader.result;
            resolve(text);
        };
        reader.readAsText(file);
    });
}
