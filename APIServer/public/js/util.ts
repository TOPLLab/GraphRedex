import * as d3 from "d3";

/**
 * Wrapper around window.fetch that returns JSON
 * and throws on nonzero output
 * @see window.fetch
 */
export function getit(input: RequestInfo, init: RequestInit): Promise<any> {
    return new Promise((resolve, reject) => {
        window
            .fetch(input, init)
            .then((x) => {
                if (x.ok) {
                    x.text().then((txt) => {
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
 * @param file file to get text from
 */
export function fileToText(file: File): Promise<string> {
    return new Promise((resolve) => {
        const reader = new FileReader();
        reader.onload = function() {
            const text = reader.result as string; // spec says it is a string
            resolve(text);
        };
        reader.readAsText(file);
    });
}
/**
 * Get a random colour with 50% saturation and lightness (good contrast on white)
 */
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

export function downloadFileLink(
    filename: string,
    dataValue: string,
    mime: string,
) {
    var a = document.createElement("a");
    a.download = filename;

    var bb = new Blob([dataValue], { type: mime });
    const url = window.URL.createObjectURL(bb);
    a.href = url;

    a.textContent = filename;
    a.classList.add("downloadLink");

    a.dataset.downloadurl = [mime, a.download, a.href].join(":");
    a.draggable = true; // Don't really need, but good practice.
    a.classList.add("dragout");

    return {
        linkEl: a,
        url: url,
    };
}

export function fracToRad(fraction: number) {
    return 2 * Math.PI * fraction;
}

export async function awaitBoolean(
    b: boolean | Promise<boolean>,
): Promise<boolean> {
    if (typeof b === "boolean") {
        return b;
    } else {
        // Promise
        return await b.catch((e) => {
            console.info(e, "assumed false");
            return false;
        });
    }
}

export async function awaitArray<T>(b: T[] | Promise<T[]>): Promise<T[]> {
    if (Array.isArray(b)) {
        return b;
    } else {
        // Promise
        return await b.catch((e) => {
            console.info(e, "assumed no options");
            return [];
        });
    }
}

/**
 * Yield the indexes of the array from boundary to middle
 * [0,1,2,3,4,5,6] will give in order:
 *   [ 0, 0 ],
     [ 6, 6 ],
     [ 1, 1 ],
     [ 5, 5 ],
     [ 2, 2 ],
     [ 4, 4 ],
     [ 3, 3 ]
 * @param array Array to loop over
 */
export function* mLast<U>(array: U[]): IterableIterator<[U, number]> {
    const n = array.length;
    for (let i = 0; i < array.length; i++) {
        const balancedi = i % 2 === 0 ? i / 2 : n - (i + 1) / 2;
        const element = array[balancedi];
        yield [element, balancedi];
    }
}

let highlightId = 0;
export function genHighlightId() {
    return `${++highlightId}`;
}

export function strToSVGElement(svg: string): SVGElement {
    const el = document.createElement("div");
    el.innerHTML = svg;
    const svgEl: SVGElement = el.children.item(0) as SVGElement;
    return svgEl;
}
