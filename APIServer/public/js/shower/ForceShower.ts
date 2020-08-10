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
                    .distance(80)
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
                x: Math.random() * 200,
                y: Math.random() * 10 - 5,
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
        // Set some heat to get the right position
        this.simulation.alpha(0.8);
        this.simulation.alphaTarget(0).restart();
        super.show(data);
    }

    public push(data: InputData<ND, ED>, startPos: string = null) {
        // We need less increase in heat for adding to an existing graph
        this.simulation.alpha(0.5);
        this.simulation.alphaTarget(0).restart();
        super.push(data, startPos);
    }

    /**
     * Increase the alpha value of the sim.
     * (keep in [0.3 - 0.9] when used)
     */
    public heat() {
        this.simulation
            .alpha(Math.max(0.3, Math.min(0.9, this.simulation.alpha() + 0.1)))
            .restart();
    }

    protected drag(): d3.DragBehavior<any, ForceShowerNode<ND>, any> {
        return d3
            .drag<any, ForceShowerNode<ND>>()
            .on("start", (d: ForceShowerNode<ND>) => {
                if (!d3.event.active) {
                    this.simulation.alpha(0.3).restart();
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
