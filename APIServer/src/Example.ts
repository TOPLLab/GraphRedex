import MyDatabase from "./Database";
import { Graph, aql } from "arangojs";
import {
    GraphEdgeCollection,
    GraphVertexCollection,
} from "arangojs/lib/cjs/graph";

export interface ExampleMeta {
    _key: string;
    _id: string;
    baseTerm: string;
}

export default class Example {
    private name: string;
    private graph: Graph;
    private baseTerm: string;
    private vertexCollection: GraphVertexCollection;
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
        this.vertexCollection = this.graph.vertexCollection(this.graph.name);
        this.edgeCollection = this.graph.edgeCollection(
            this.graph.name + "-reductions",
        );
    }

    /**
     * Gets the outbound nodes within `steps` steps from any of the nodes in
     * bases (specified by _key)
     * @param bases the _key's of the nodes to start form (graph will be prepended)
     * @param steps number of reductions
     */
    public async extend(bases: string[], steps: number = 1) {
        if (steps < 0) {
            throw "steps must be positive";
        }
        const fullBaseNames = bases.map((x) => this.name + "/" + x);
        const qry = aql`
        LET nodes = (
            FOR docId IN ${fullBaseNames}
                FOR v IN 0..${steps}
                    OUTBOUND docId GRAPH ${this.name}
                    OPTIONS {bfs:true,uniqueVertices: 'global'}
                    RETURN DISTINCT v)
        LET edges = (
            FOR a in nodes
                FOR e IN ${this.edgeCollection}
                    FILTER  e._from == a._id OR e._to == a._id
                        RETURN DISTINCT e)
        RETURN {nodes,edges}`;

        return await this.db.ro
            .query(qry)
            .then((cursor) => cursor.all())
            .then((keys) =>
                Object.assign({ meta: { baseTerms: fullBaseNames } }, keys[0]),
            );
    }

    /**
     * Execute a query on the graph database
     * Binding
     *  - @@nodes to the collection of nodes
     *  - @@edges to the collection of edges
     *  - @start  to the id of the start node
     *  - @graph  to the graph of the start node
     *
     * @param qry
     */
    public async qry(qry: string): Promise<any[]> {
        const binds = {};
        const availibleBind = {
            "@nodes": this.vertexCollection.name,
            "@edges": this.edgeCollection.name,
            "graph": this.graph.name,
            "start": this.baseTerm,
        };

        for (const [key, value] of Object.entries(availibleBind)) {
            if (qry.includes("@" + key)) {
                binds[key] = value;
            }
        }

        return await this.db.ro
            .query(qry, binds)
            .then((cursor) => cursor.all())
            .then((keys) => Object.assign(keys));
    }

    /**
     * Get nodes and edges starting formthe base node of the graph
     *  @param [steps=300] number of steps
     */
    public async showAll(steps: number = 300) {
        return await this.extend([this.baseTerm.split("/").pop()], steps);
    }
}
