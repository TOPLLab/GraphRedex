/// <reference path="./ShowerTypes.d.ts"/>

/**
 * D3 force that calculates the Depth of all the nodes according to the links
 * and pulls the nodes to x pos `intervalLen` * depth.
 *
 * @param dist      Distance between nodes of subsequent depths
 * @param getStart  function that returns the current start node id
 * @todo  Refactor into class once classes can be callable
 */
export default function (dist: number = 80, getStart: () => string) {
    // private fields
    let strength: number = 0.1,
        nodes: ShowerNode<any>[],
        hasLinks = false,
        nodeDepth: number[],
        nodesIndexes: Map<string, number>;

    // the main force function
    // Does nothing if there is no startnode, or no links
    const force = (alpha: number) => {
        if (hasLinks && getStart()) {
            for (let i = 0, n = nodes.length; i < n; ++i) {
                let node: any = nodes[i];
                node.vx += (nodeDepth[i] * dist - node.x) * strength * alpha;
            }
        }
    };

    // Initialiser, store the nodes (called by d3)
    force.initialize = function (inputNodes: ShowerNode<any>[]) {
        nodes = inputNodes;
        nodesIndexes = new Map(
            inputNodes.map<[string, number]>((e, i) => [e.data._id, i]),
        );
    };

    /**
     * Setter and getter for the force (default 0.1)
     * @param strength optional new strength
     */
    force.strength = function (newStrength: number) {
        return arguments.length ? (strength = newStrength) : strength;
    };

    /**
     * DFS to set depths
     */
    function setDepths(cur: number, linkMap: number[][], depth: number) {
        if (!(nodeDepth[cur] && nodeDepth[cur] <= depth)) {
            // not visited with lower depth
            nodeDepth[cur] = depth;

            const nextDepth = depth + 1;
            for (let child of linkMap[cur]) {
                setDepths(child, linkMap, nextDepth);
            }
        }
    }

    force.links = (
        links: { source: { id: string }; target: { id: string } }[],
    ) => {
        const startNode = getStart() ?? null;
        const n = nodes.length;
        if (startNode !== null && n > 0) {
            const linkMap: number[][] = Array.from({ length: n }, () => []);

            for (const { source, target } of links) {
                const s = nodesIndexes.get(source.id) ?? null;
                const t = nodesIndexes.get(target.id) ?? null;
                if (s !== null && t !== null) {
                    linkMap[s].push(t);
                }
            }

            // prepare node depts map
            nodeDepth = new Array(n).fill(null);

            setDepths(nodesIndexes.get(getStart()), linkMap, 0);

            hasLinks = true;
        }
    };

    return force;
}
