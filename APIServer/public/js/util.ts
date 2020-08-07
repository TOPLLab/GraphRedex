import * as d3 from "d3";
import { TermDiffElem } from "./termDiff";

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
        reader.onload = function () {
            const text = reader.result as string; // spec says it is a string
            resolve(text);
        };
        reader.readAsText(file);
    });
}
/**
 * Get a random colour with 50% saturation and lightness (good contrast on
 * white)
 */
export function mkRandomColorGenerator(): () => {
    hex: string;
    hexFull: string;
    d3: d3.Color;
} {
    let hues = [];
    return () => {
        let degree = Math.floor(Math.random() * 360);
        switch (hues.length) {
            case 0:
                // push
                break;
            case 1:
                degree = (180 + hues[0]) % 360;
                break;
            default:
                let [start, , size] = hues
                    .map((v, i, a) => [v, a[(i + 1) % a.length]])
                    .map(([a, b]) => [a, b, a < b ? b - a : b - a + 360])
                    .sort((a, b) => a[2] - b[2])
                    .pop();
                degree = Math.round((360 + start + size / 2) % 360);
        }
        hues.push(degree);
        hues = hues.sort((a, b) => a - b);

        const d3col = d3.color(`hsl(${degree}, 50%, 50%)`).rgb();
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
    };
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

const termDiffMaker: TermDiffElem<HTMLElement> = {
    join: (content: HTMLElement[], different: boolean, depth: number) => {
        // Join elements as child of a tagName element.
        const joinedAs = (tagName: "DIV" | "SPAN") => {
            const e = document.createElement(tagName);
            e.classList.add(different ? "diff" : "same");

            // Add some extra spaces arround elements
            e.append(...[].concat(...content.map((x) => [" ", x, " "])));
            return e;
        };

        return content.every((x) => x.tagName === "SPAN") &&
            content.map((x) => x.textContent).join(" ").length < 25 - depth * 2
            ? joinedAs("SPAN") // as span if shorter than 25 char
            : joinedAs("DIV"); // otherwise as div
    },
    convert: (content: string, different: boolean) => {
        const e = document.createElement("SPAN");
        e.classList.add(different ? "diff" : "same");
        e.textContent = content;
        return e;
    },
};

export { termDiffMaker };
