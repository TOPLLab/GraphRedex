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

export function randomColor(): { hex: string; hexFull: string; d3: d3.Color } {
    const d3col = d3
        .color(`hsl(${Math.floor(Math.random() * 360)}, 50%, 50%)`)
        .rgb();
    let r = Math.round(d3col.r).toString(16);
    let g = Math.round(d3col.g).toString(16);
    let b = Math.round(d3col.b).toString(16);
    if (d3col.r < 16) {
        r = "0" + r;
    }
    if (d3col.g < 16) {
        g = "0" + g;
    }
    if (d3col.b < 16) {
        b = "0" + b;
    }
    const hex = r + g + b;
    return { hex: hex, hexFull: "#" + hex, d3: d3col };
}
