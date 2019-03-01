export type TermDiffMarkup = (content: string, different: boolean) => string;

const f: TermDiffMarkup = (x, d) =>
    `<span class="${d ? "diff" : "same"}">${x}</span>`;

export default function termDiff(
    t1: string,
    t2: string,
    markup: TermDiffMarkup = f,
) {
    const d1 = destructureTerm(t1);
    const d2 = destructureTerm(t2);
    structureDiff(d1, d2);
    return [visualise(d1, markup), visualise(d2, markup)];
}

function visualise(d1: Decoded, f: TermDiffMarkup) {
    if (isArray(d1.content)) {
        return f(
            d1.content.map((x) => visualise(x, f)).join(" "),
            d1.isDifferent,
        );
    } else {
        return f(d1.content, d1.isDifferent);
    }
}

type Decoded = {
    isDifferent: boolean;
    content: Array<Decoded> | string;
};

function isArray(arg: any): arg is Array<Decoded | string> {
    return Array.isArray(arg);
}

function structureDiff(d1: Decoded, d2: Decoded) {
    if (isArray(d1.content) && isArray(d2.content)) {
        if (d1.content.length === d2.content.length) {
            for (let index = 0; index < d1.content.length; index++) {
                structureDiff(d1.content[index], d2.content[index]);
            }
            return false;
        } else {
            d1.isDifferent = true;
            d2.isDifferent = true;
            return true;
        }
    } else {
        if (isArray(d1.content) || isArray(d2.content)) {
            // mismatch
            d1.isDifferent = true;
            d2.isDifferent = true;
            return true;
        } else {
            // strings
            const different = d1.content !== d2.content;
            d1.isDifferent = different;
            d2.isDifferent = different;
            return different;
        }
    }
}

type RawDecoded = RawDecodedArray | string | null;
interface RawDecodedArray extends Array<RawDecoded> {}

export function destructureTerm(data: string): Decoded {
    let d: RawDecoded = [];
    let ptrs = [d];

    let curPtr = () => ptrs[ptrs.length - 1];
    let addChar = (c: string) => {
        const cur = curPtr();
        if (cur.length > 0 && typeof cur[cur.length - 1] === "string") {
            cur[cur.length - 1] += c;
        } else {
            cur.push(c);
        }
    };

    for (const c of data) {
        switch (c) {
            case "(":
                let newC = [];
                curPtr().push(newC);
                ptrs.push(newC);
                curPtr().push(c);
                curPtr().push(null);

                break;
            case ")":
                curPtr().push(null);
                curPtr().push(c);
                ptrs.pop();
                break;
            case " ":
            case "\t":
            case "\n":
                curPtr().push(null);

                break;
            default:
                addChar(c);
        }
    }

    return rmnull(d);
}

function rmnull(e: RawDecoded): Decoded {
    if (Array.isArray(e)) {
        return {
            isDifferent: false,
            content: e.filter((x) => x !== null).map((x) => rmnull(x)),
        };
    }
    return {
        isDifferent: false,
        content: e,
    };
}
