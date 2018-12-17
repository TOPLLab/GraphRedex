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
    nodeMaker?: (nodes: d3.Selection<any, ShowerNode, any, any>) => void;
    nodeUpdate?: (nodes: d3.Selection<any, ShowerNode, any, any>) => void;
    rootId?: string;
}
