import MyDatabase from "./Database";
import { Graph } from "arangojs";
import {
    GraphEdgeCollection,
    GraphVertexCollection,
} from "arangojs/lib/cjs/graph";

export interface ExampleMeta {
    _key: string;
    _id: string;
    baseTerm: string;
    baseTermString: string;
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
}
