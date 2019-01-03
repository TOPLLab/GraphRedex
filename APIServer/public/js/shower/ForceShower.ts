import Shower from "./Shower";
import treeForce from "./treeForce";
import * as d3 from "d3";

interface GRND extends NodeData {
    _id: string;
    _key: string;
    term: string;
    _stuck: boolean;
    _limited?: boolean;
    _expanded: boolean;
}
interface GRED extends EdgeData {
    _id: string;
    _from: string;
    _to: string;
    reduction: string;
    _real: boolean;
}

interface ForceShowerNode<ND extends GRND> extends ShowerNode<ND> {
    id: string;
    data: ND;
    x?: any;
    y?: any;
    fx?: number;
    fy?: number;
}
interface ForceShowerEdge<ND extends GRND, ED extends GRED>
    extends ShowerEdge<ND, ED> {
    source: ShowerNode<ND>;
    target: ShowerNode<ND>;
    data: ED;
}

export default class ForceShower<
    ND extends GRND,
    ED extends GRED
> extends Shower<ND, ED, ForceShowerNode<ND>, ForceShowerEdge<ND, ED>> {
    /** d3 force simulation that puts the nodes in the right place */
    private simulation: d3.Simulation<any, any>;

    constructor(
        svg: any,
        showerConfig: ShowerConfigFull<
            ND,
            ED,
            ForceShowerNode<ND>,
            ForceShowerEdge<ND, ED>
        >,
    ) {
        super(svg, showerConfig);
        this.simulation = d3
            .forceSimulation()
            .force(
                "link",
                d3
                    .forceLink<ForceShowerNode<ND>, ForceShowerEdge<ND, ED>>()
                    .distance((d) => (d.data._real ? 60 : 90))
                    .strength(1.5)
                    .id((d: ForceShowerNode<ND>) => d.id),
            )
            .force("charge", d3.forceManyBody().strength(-60))
            .force("collide", d3.forceCollide(16).strength(0.7))
            .force("long", d3.forceY().strength(0.01));

        this.simulation.force(
            "dfsDepth",
            treeForce(80, () => this.config.rootId),
        );
    }

    public update() {
        // Redefine and restart simulation
        this.simulation.nodes(this.data.nodes).on("tick", () => this.ticked());

        this.simulation.velocityDecay(0.1);
        const linkForce: d3.ForceLink<any, any> = this.simulation.force("link");
        linkForce.links(this.data.edges);

        if (this.config.rootId) {
            const dfsForce: any = this.simulation.force("dfsDepth");
            dfsForce.links(this.data.edges);
        }
        super.update();
    }

    protected convertNode(
        n: ND,
        startPos: { x?: number; y?: number } = null,
    ): ForceShowerNode<ND> {
        if (startPos) {
            return {
                id: n._id,
                data: n,
                x: startPos.x || 0,
                y: startPos.y || 0,
                shown: true,
                fx: null,
                fy: null,
            };
        } else {
            return {
                id: n._id,
                data: n,
                shown: true,
                fx: null,
                fy: null,
            };
        }
    }

    protected convertEdge(e: ED): ForceShowerEdge<ND, ED> {
        return {
            source: this.nodeMap.get(e._from),
            target: this.nodeMap.get(e._to),
            data: e,
        };
    }

    public show(data: InputData) {
        this.simulation.alphaTarget(0.3).restart();
        super.show(data);
        this.heatFor();
    }

    public push(data: InputData, startPos: string = null) {
        this.simulation.alphaTarget(0.3).restart();
        super.push(data, startPos);
        this.heatFor();
    }

    private heatTimeout = null;
    /**
     * Set the alphaTarget to 0.3 for some time (reset to 0 afterwards)
     */
    public heatFor(timeout: number = 5000) {
        window.clearTimeout(this.heatTimeout);
        this.simulation.alphaTarget(0.3).restart();
        this.heatTimeout = window.setTimeout(() => {
            this.simulation.alphaTarget(0);
        }, timeout);
    }

    protected drag(): d3.DragBehavior<any, ForceShowerNode<ND>, any> {
        return d3
            .drag<any, ForceShowerNode<ND>>()
            .on("start", (d: ForceShowerNode<ND>) => {
                if (!d3.event.active) {
                    this.simulation.alphaTarget(0.3).restart();
                }
                d.fx = d.x;
                d.fy = d.y;
            })
            .on("drag", (d: ForceShowerNode<ND>) => {
                d.fx = d3.event.x;
                d.fy = d3.event.y;
            })
            .on("end", () => {
                if (!d3.event.active) {
                    this.simulation.alphaTarget(0);
                }
            });
    }
}
