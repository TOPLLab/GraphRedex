/// <reference path="./ShowerTypes.d.ts"/>

/**
 * D3 force that calculates the Depth of all the nodes accorging to the links
 * and pulls the nodes to x pos `intervalLen` * depth.
 *
 * @param intervalLen      Distance between nodes of subsequent depths
 * @param startNodeGetter  function that returns the current startnode id
 * @todo  Maybe we need a deph getter here...
 */
export default function(
    intervalLen: number = 80,
    startNodeGetter: () => string,
) {
    let strength: number = 0.1,
        nodes: ShowerNode<any>[],
        hasLinks = false,
        nodeDepth: number[],
        nodesIndexes: Map<string, number>;

    const force = (alpha: number) => {
        if (!hasLinks || !startNodeGetter()) {
            return;
        }
        for (var i = 0, n = nodes.length, node; i < n; ++i) {
            (node = nodes[i]),
                (node.vx +=
                    (nodeDepth[i] * intervalLen - node.x) * strength * alpha);
        }
    };

    force.initialize = function(inputNodes: ShowerNode<any>[]) {
        nodes = inputNodes;
        nodesIndexes = new Map(
            inputNodes.map<[string, number]>((e, i) => [e.data._id, i]),
        );
    };

    /**
     * Setter and getter for the force (deault 0.1)
     * @param strength optional new strenght
     */
    force.strength = function(newStrength: number) {
        return arguments.length ? (strength = newStrength) : strength;
    };

    function setDepths(cur: number, linkMap: number[][], depth: number) {
        if (!(nodeDepth[cur] && nodeDepth[cur] <= depth)) {
            // not visited with lower depth
            nodeDepth[cur] = depth;
            const n = linkMap[cur].length;
            const nextDepth = depth + 1;
            for (let i = 0; i < n; ++i) {
                setDepths(linkMap[cur][i], linkMap, nextDepth);
            }
        }
    }

    force.links = (
        links: { source: { id: string }; target: { id: string } }[],
    ) => {
        if (startNodeGetter()) {
            const n = nodes.length;
            const linkMap = new Array(nodes.length);
            for (let i = 0; i < n; ++i) {
                linkMap[i] = [];
            }
            for (const { source, target } of links) {
                const s = nodesIndexes.get(source.id);
                const t = nodesIndexes.get(target.id);
                if (s !== undefined && t !== undefined) {
                    linkMap[s].push(t);
                }
            }

            nodeDepth = new Array(n);
            for (let i = 0; i < n; ++i) {
                nodeDepth[i] = null;
            }

            setDepths(nodesIndexes.get(startNodeGetter()), linkMap, 0);

            hasLinks = true;
        }
    };

    return force;
}
