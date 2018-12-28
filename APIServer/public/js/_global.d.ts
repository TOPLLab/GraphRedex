interface APIDoTermResult {
    lang: any;
    term: string;
    output: string;
    example: ExampleMeta;
}

export interface ExampleMeta {
    lang: string;
    _key: string;
    _id: string;
    baseTerm: string;
    baseTermString: string;
}

interface TermMeta {
    _id: string;
    _key: string;
    term: string;
    _expanded: boolean;
}
