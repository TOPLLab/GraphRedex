export type TermDiffMarkup = (content: string, different: boolean) => string;

const f: TermDiffMarkup = (x, d) =>
    `<span class="${d ? "diff" : "same"}">${x}</span>`;

export default function termDiff(
    t1: string,
    t2: string,
    markup: TermDiffMarkup = f,
): string {
    const d1 = destructureTerm(t1);
    const d2 = destructureTerm(t2);
    structureDiff(d1, d2);
    return visualise(d1, t1, markup).text;
}

export function doubleTermDiff(
    t1: string,
    t2: string,
    markup: TermDiffMarkup = f,
) {
    const d1 = destructureTerm(t1);
    const d2 = destructureTerm(t2);
    structureDiff(d1, d2);
    return [visualise(d1, t1, markup).text, visualise(d2, t2, markup).text];
}

function visualise(d1: Decoded, t1: string, f: TermDiffMarkup): TextPiece {
    if (isArray(d1.content)) {
        if (d1.content.length > 0) {
            let result = visualise(d1.content[0], t1, f);
            for (let i = 1; i < d1.content.length; i++) {
                const cur = visualise(d1.content[i], t1, f);
                while (result.to + 1 < cur.from) {
                    result.to++;
                    result.text += t1[result.to];
                } // result.to + 1 == cur.from
                result.text += cur.text;
                result.to = cur.to;
            }
            result.text = f(result.text, d1.isDifferent);
            return result;
        } else {
            throw "Unexpected end of diff";
        }
    } else {
        return {
            text: f(d1.content.text, d1.isDifferent),
            from: d1.content.from,
            to: d1.content.to,
        };
    }
}

type Decoded = {
    isDifferent: boolean;
    content: Array<Decoded> | TextPiece;
};

function isArray(arg: any): arg is Array<Decoded | TextPiece> {
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
            const maxIndex = Math.min(d1.content.length, d2.content.length);
            for (let index = 0; index < maxIndex; index++) {
                structureDiff(d1.content[index], d2.content[index]);
            }
            for (let index = maxIndex; index < d1.content.length; index++) {
                d1.content[index].isDifferent = true;
            }
            for (let index = maxIndex; index < d2.content.length; index++) {
                d2.content[index].isDifferent = true;
            }
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
            const different = d1.content.text !== d2.content.text;
            d1.isDifferent = different;
            d2.isDifferent = different;
            return different;
        }
    }
}

type RawDecoded =
    | RawDecodedArray
    | { text: string; from: number; to: number }
    | null;
type TextPiece = { text: string; from: number; to: number };
interface RawDecodedArray extends Array<RawDecoded> {}

function isTextpiece(arg: RawDecoded): arg is TextPiece {
    return arg !== null && "text" in arg;
}

export function destructureTerm(data: string): Decoded {
    let d: RawDecoded = [];
    let ptrs = [d];

    let curPtr = () => ptrs[ptrs.length - 1];
    let addChar = (c: string, pos: number, join = true) => {
        const cur = curPtr();
        if (join && cur.length > 0) {
            const curLast = cur[cur.length - 1];
            if (isTextpiece(curLast)) {
                curLast.text += c;
                curLast.to = pos;
                return;
            }
        }
        cur.push({ text: c, from: pos, to: pos });
    };

    for (let i = 0; i < data.length; i++) {
        const c = data[i];
        switch (c) {
            case "(":
                let newC = [];
                curPtr().push(newC);
                ptrs.push(newC);
                addChar(c, i, false);
                curPtr().push(null);

                break;
            case ")":
                curPtr().push(null);
                addChar(c, i);
                ptrs.pop();
                break;
            case " ":
            case "\t":
            case "\n":
                curPtr().push(null);

                break;
            default:
                addChar(c, i);
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
