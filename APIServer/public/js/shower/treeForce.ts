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
    var strength = 0.1,
        nodes,
        hasLinks = false,
        xz,
        nodesIndexes;

    const force = (alpha: number) => {
        if (!hasLinks || !startNodeGetter()) {
            return;
        }
        for (var i = 0, n = nodes.length, node; i < n; ++i) {
            (node = nodes[i]),
                (node.vx += (xz[i] * intervalLen - node.x) * strength * alpha);
        }
    };

    function initialize() {
        if (!nodes) {
            return;
        }
    }

    force.initialize = function(_: ShowerNode[]) {
        nodes = _;
        nodesIndexes = new Map(
            _.map<[string, number]>((e, i) => [e.data._id, i]),
        );
        initialize();
    };

    /**
     * Setter and getter for the force (deault 0.1)
     * @param strength optional new strenght
     */
    force.strength = function(_: number) {
        return arguments.length ? (strength = _) : strength;
    };

    function setDepths(cur: number, linkMap: number[][], depth: number) {
        if (!(xz[cur] && xz[cur] <= depth)) {
            // not visited with lower depth
            xz[cur] = depth;
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

            xz = new Array(n);
            for (let i = 0; i < n; ++i) {
                xz[i] = null;
            }

            setDepths(nodesIndexes.get(startNodeGetter()), linkMap, 0);

            hasLinks = true;
            console.log(this, nodes, linkMap, xz);
            initialize();
        }
    };

    return force;
}
