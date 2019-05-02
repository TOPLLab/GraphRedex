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
    shown: boolean;
}
interface ShowerEdge<ND extends NodeData, ED extends EdgeData> {
    source: ShowerNode<ND>;
    target: ShowerNode<ND>;
    data: ED;
}

interface InputData<ND extends NodeData, ED extends EdgeData> {
    nodes: ND[];
    edges: ED[];
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

type ShowerOptionData = {
    /** Name to be shown */
    name: string;
    /** Importance of the option */
    size?: number;
    /** action to carry out when the option is selected
     * @return true if the node should remain open
     */
    action: () => boolean | Promise<boolean>;
    /** Icon to show */
    icon?: string;
};

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
    /** function called when a node is selected (e.g. by clicking or traversal)
     * @return true if nodeOptions should be shown
     */
    nodeSelected?: (node: N) => boolean;
    /** function that returns the options for a node */
    nodeOptions?: (
        node: N,
    ) => ShowerOptionData[] | Promise<ShowerOptionData[]> | null;
    /** function called when a node is selected (e.g. by clicking or traversal) */
    edgeSelected?: (edge: E) => void;
    /** the root node of the visualization that will be used to find out the
     * depth of other nodes.
     */
    rootId?: string;
}
