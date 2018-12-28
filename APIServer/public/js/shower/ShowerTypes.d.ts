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

interface ShowerNode<ND extends NodeData> {
    id: string;
    data: ND;
    x?: any;
    y?: any;
    fx?: number;
    fy?: number;
    shown: boolean;
}
interface ShowerEdge<ND extends NodeData, ED extends EdgeData> {
    source: ShowerNode<ND>;
    target: ShowerNode<ND>;
    data: ED;
}

interface InputData {
    nodes: NodeData[];
    edges: EdgeData[];
}

interface ShowerData<N, E> {
    nodes: N[];
    edges: E[];
}

type ShowerConfig<ND extends NodeData, ED extends EdgeData> = ShowerConfigFull<
    ND,
    ED,
    ShowerNode<ND>,
    ShowerEdge<ND, ED>
>;

interface ShowerConfigFull<
    ND extends NodeData,
    ED extends EdgeData,
    N extends ShowerNode<ND>,
    E extends ShowerEdge<ND, ED>
> {
    /** function that is called on newly created nodes in the graph */
    nodeMaker?: (nodes: d3.Selection<any, N, any, any>) => void;
    /** function that is called on all nodes every time there is an update */
    nodeUpdate?: (nodes: d3.Selection<any, N, any, any>) => void;
    /** function called when a node is selected (eg by clicking or traversal) */
    nodeSelected?: (node: N) => void;
    /** function called when a node is selected (eg by clicking or traversal) */
    edgeSelected?: (edge: E) => void;
    /** the root node of the visualisation that will be used to find out the
     * depth of other nodes.
     */
    rootId?: string;
}
