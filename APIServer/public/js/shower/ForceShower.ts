import * as d3 from "d3";
import Shower from "./Shower";
import treeForce from "./treeForce";

interface ForceShowerNode<ND extends NodeData> extends ShowerNode<ND> {
    id: string;
    data: ND;
    x?: any;
    y?: any;
    fx?: number;
    fy?: number;
    ix?: number;
    iy?: number;
}
interface ForceShowerEdge<ND extends NodeData, ED extends EdgeData>
    extends ShowerEdge<ND, ED> {
    source: ShowerNode<ND>;
    target: ShowerNode<ND>;
    data: ED;
}

export default class ForceShower<
    ND extends NodeData,
    ED extends EdgeData
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
                    .distance((d) => (d.data._real ? 80 : 90))
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
        this.svgRoot.on("click", () => {
            this.selectNode(null);
            console.log("reset selected node");
        });
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

    public swapFor<N extends ShowerNode<ND>, E extends ShowerEdge<ND, ED>>(
        constructor: (
            element: SVGSVGElement,
            config: ShowerConfigFull<ND, ED, N, E>,
            data: InputData<ND, ED>,
        ) => Shower<ND, ED, N, E>,
    ) {
        window.clearTimeout(this.heatTimeout);
        this.simulation.stop();
        this.simulation = null;
        super.swapFor(constructor);
        this.nodeMap = null;
        this.data = null;
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

    public show(data: InputData<ND, ED>) {
        this.simulation.alphaTarget(0.3).restart();
        super.show(data);
        this.heatFor();
    }

    public push(data: InputData<ND, ED>, startPos: string = null) {
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
        this.simulation
            .alphaTarget(this.heatTimeout === null ? 0.3 : 0.7)
            .restart();
        this.heatTimeout = window.setTimeout(() => {
            this.heatTimeout = null;
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
                d.ix = d.ix || d.x;
                d.iy = d.iy || d.y;
            })
            .on("drag", (d: ForceShowerNode<ND>) => {
                if (
                    d.fy !== null ||
                    d.fy !== null ||
                    Math.abs(d.ix - d3.event.x) + Math.abs(d.iy - d3.event.y) >
                        7.5
                ) {
                    d.fx = d3.event.x;
                    d.fy = d3.event.y;
                } else {
                    d.fx = null;
                    d.fy = null;
                }
            })
            .on("end", () => {
                if (!d3.event.active) {
                    this.simulation.alphaTarget(0);
                }
            });
    }
}
