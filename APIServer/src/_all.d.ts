interface Blob {
    /* for arango */
}

interface MulterDiskFile {
    fieldname: string;
    originalname: string;
    encoding: string;
    mimetype: string;
    size: number;
    destination: string;
    filename: string;
    path: string;
}

interface Language {
    name: string;
    path: string;
    dir: string;
    onDisk?: boolean;
    _key?: Number;
    debugging: boolean;
}

interface TermMeta {
    _id: string;
    _key: string;
    term: string;
}
