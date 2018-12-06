import * as _d3 from "d3";

declare global {
    const d3: any;
}

interface APIDoTermResult {
    lang: any;
    term: string;
    output: string;
    example: ExampleMeta;
}

export interface ExampleMeta {
    _key: string;
    _id: string;
    baseTerm: string;
    baseTermString: string;
}
