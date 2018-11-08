import MyDatabase from "./Database";
import { Graph, aql } from "arangojs";
import { GraphEdgeCollection } from "arangojs/lib/cjs/graph";

export interface ExampleMeta {
    _key: string;
    _id: string;
    baseTerm: string;
}

export default class Example {
    private name: string;
    private graph: Graph;
    private baseTerm: String;
    private edgeCollection: GraphEdgeCollection;
    private db: MyDatabase;

    /**
     *
     * @param database A database connection
     * @param meta     The metadata of an example
     */
    constructor(database: MyDatabase, meta: ExampleMeta) {
        this.baseTerm = meta.baseTerm;
        this.db = database;
        this.name = this.baseTerm.split("/")[0];
        this.graph = database.ro.graph(this.name);
        this.baseTerm = this.baseTerm;
        this.edgeCollection = this.graph.edgeCollection(
            this.graph.name + "-reductions",
        );
    }

    /**
     *
     * @param from  The start term, must be in the example, should be of the from "example-1/5445454" (the examples base term)
     * @param steps
     */
    public async showAll(from: String = null, steps: number = 200) {
        if (from === null) {
            from = this.baseTerm;
        }

        const qry = aql`
        LET docId = ${this.baseTerm}
        LET nodes = (FOR v IN 0..${steps}
            OUTBOUND docId GRAPH ${this.name}
            OPTIONS {bfs:true,uniqueVertices: 'global'}
            RETURN v)
        LET nodesID = (FOR v IN nodes RETURN v._id)
        LET edges = (
            FOR a in nodesID FOR e IN ${this.edgeCollection}
                FILTER
                    e._from == a AND
                    POSITION(nodesID,e._to) == true
            RETURN DISTINCT e)
        RETURN {nodes,edges}`;

        return await this.db.ro
            .query(qry)
            .then((cursor) => cursor.all())
            .then((keys) =>
                Object.assign({ meta: { baseTerm: this.baseTerm } }, keys[0]),
            );
    }
}
