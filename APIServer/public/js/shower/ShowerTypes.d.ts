/// <reference path="../_global.d.ts"/>
interface NodeData {
    _id: string;
    term: string;
}
interface EdgeData {
    _id: string;
    _from: string;
    _to: string;
    reduction: string;
    _real: boolean;
}

interface ShowerNode {
    id: string;
    data: NodeData;
    x?: any;
    y?: any;
    fx?: number;
    fy?: number;
}
interface ShowerEdge {
    source: ShowerNode;
    target: ShowerNode;
    data: EdgeData;
}

interface InputData {
    nodes: NodeData[];
    edges: EdgeData[];
}

interface ShowerData {
    nodes: ShowerNode[];
    edges: ShowerEdge[];
}

interface ShowerConfig {
    /** function that is called on newly created nodes in the graph */
    nodeMaker?: (nodes: d3.Selection<any, ShowerNode, any, any>) => void;
    /** function that is called on all nodes every time there is an update */
    nodeUpdate?: (nodes: d3.Selection<any, ShowerNode, any, any>) => void;
    /** the root node of the visualisation that will be used to find out the
     * depth of other nodes.
     */
    rootId?: string;
}
