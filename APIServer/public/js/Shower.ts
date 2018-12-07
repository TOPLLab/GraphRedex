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
}

export default class Shower {
    private svgRoot: any;
    private width: number = 1000;
    private height: number = 1000;
    private zoomHandler: d3.ZoomBehavior<any, any>;
    private defs: any;
    private scene: any;
    private parts: {
        nodes: d3.Selection<any, ShowerNode, any, any>;
        arrows: d3.Selection<any, ShowerEdge, any, any>;
        texts: d3.Selection<any, ShowerEdge, any, any>;
    };
    private data: ShowerData;
    private nodeMap: Map<string, ShowerNode>;
    private simulation: d3.Simulation<any, any>;
    private config: ShowerConfig;

    constructor(svg: any, showerConfig: ShowerConfig) {
        this.svgRoot = d3.select(svg);
        this.svgRoot
            .attr("width", this.width)
            .attr("height", this.height)
            .attr("viewBox", [
                -this.width / 2,
                -this.height / 2,
                this.width,
                this.height,
            ])
            .call(
                (this.zoomHandler = d3.zoom().on("zoom", () => {
                    this.scene.attr("transform", d3.event.transform);
                })),
            );
        this.defs = this.svgRoot.append("svg:defs");
        this.scene = this.svgRoot.append("g");

        this.data = { nodes: [], edges: [] };

        this.config = showerConfig;

        this.parts = {
            arrows: this.scene
                .append("g")
                .classed("graph-arrows", true)
                .selectAll("path")
                .data([]),
            texts: this.scene
                .append("g")
                .classed("graph-texts", true)
                .selectAll("text")
                .data([]),
            nodes: this.scene
                .append("g")
                .classed("graph-nodes", true)
                .selectAll("circle")
                .data([]),
        };

        this.simulation = d3
            .forceSimulation()
            .force(
                "link",
                d3
                    .forceLink()
                    .distance((d: ShowerEdge) => (d.data._real ? 60 : 90))
                    .strength(1.5)
                    .id((d: ShowerNode) => d.id),
            )
            .force("charge", d3.forceManyBody().strength(-30))
            .force("collide", d3.forceCollide(16).strength(0.7))
            //.force("center", d3.forceCenter())
            .force("center", d3.forceY().strength(0.01));
    }

    private update() {
        // Redefine and restart simulation
        this.simulation.nodes(this.data.nodes).on("tick", () => this.ticked());

        this.simulation.velocityDecay(0.1);
        const linkForce: d3.ForceLink<any, any> = this.simulation.force("link");
        linkForce.links(this.data.edges);

        const p = this.parts;

        // arrows
        p.arrows = p.arrows.data(this.data.edges);
        const arrowsEnter = p.arrows
            .enter()
            .append("path")
            .attr("stroke", (d) => this.getRandCol(d.data.reduction))
            .attr("stroke-dasharray", (d) => (d.data._real ? null : "5,5"))
            .attr(
                "marker-end",
                (d) => "url(#" + this.getRandCol(d.data.reduction, true) + ")",
            );
        p.arrows = arrowsEnter.merge(p.arrows);
        p.arrows.exit().remove();

        // texts
        p.texts = p.texts.data(this.data.edges);
        const textEnter = p.texts
            .enter()
            .append("text")
            .attr("text-anchor", "middle")
            .attr("fill", (d) => this.getRandCol(d.data.reduction))
            .html((d) => "<tspan dy='-5'>" + d.data.reduction + "</tspan>");
        p.texts = textEnter.merge(p.texts);
        p.texts.exit().remove();

        // nodes
        p.nodes = p.nodes.data(this.data.nodes);
        const nodeEnter = p.nodes
            .enter()
            .append("circle")
            .classed("term-node", true)
            .attr("r", 10)
            .call(this.drag());
        if ("nodeMaker" in this.config) {
            this.config.nodeMaker(nodeEnter);
        }
        p.nodes = nodeEnter.merge(p.nodes);
        p.nodes.exit().remove();
    }

    private ticked() {
        console.log("sim");
        this.parts.arrows.attr("d", (d) => {
            const dx = d.target.x - d.source.x;
            const dy = d.target.y - d.source.y;
            return `M ${d.source.x} ${d.source.y} q ${dx / 2} ${dy /
                2} ${dx} ${dy}`;
        });

        this.parts.texts
            .attr("x", (d) => (d.source.x + d.target.x) / 2)
            .attr("y", (d) => (d.source.y + d.target.y) / 2)
            .attr(
                "transform",
                (d) => `rotate(${(180 *
                    Math.atan(
                        (d.source.y - d.target.y) / (d.source.x - d.target.x),
                    )) /
                    Math.PI},
                    ${(d.source.x + d.target.x) / 2},
                    ${(d.source.y + d.target.y) / 2}
                    )`,
            );

        this.parts.nodes.attr("cx", (d) => d.x).attr("cy", (d) => d.y);
    }

    private convertNode(n: NodeData): any {
        return { id: n._id, data: n, vx: 5 };
    }
    private convertEdge(d: EdgeData): ShowerEdge {
        return {
            source: d._from as any,
            target: d._to as any,
            data: d,
        };
    }

    /**
     * shows data
     */
    public show(data: InputData) {
        this.data.nodes = data.nodes.map(this.convertNode);
        this.nodeMap = new Map(
            this.data.nodes.map<[string, ShowerNode]>((x) => [x.id, x]),
        );
        this.data.edges = data.edges
            .filter((e) => this.nodeMap.has(e._from) && this.nodeMap.has(e._to))
            .map(this.convertEdge);
        this.update();
        console.log("GO", this.data);
        this.simulation.alphaTarget(0.3).restart();
    }

    public push(data: InputData, startPos: string = null) {
        this.simulation.alphaTarget(0.3).restart();
        const newNodes = data.nodes
            .filter((n) => !this.nodeMap.has(n._id))
            .map(this.convertNode);
        if (startPos) {
            const startNode = this.nodeMap.get(startPos);
            if (startNode) {
                newNodes.forEach((n) => {
                    n.x = startNode.x;
                    n.y = startNode.y;
                });
            }
        }
        this.data.nodes.push(...newNodes);
        newNodes.forEach((n) => {
            this.nodeMap.set(n.id, n);
        });
        this.data.edges.push(
            ...data.edges
                .filter(
                    (e) => this.nodeMap.has(e._from) && this.nodeMap.has(e._to),
                )
                .map(this.convertEdge),
        );
        this.update();
        console.log("GO", this.data);
    }

    public resetZoom() {
        this.svgRoot.call(this.zoomHandler.transform, d3.zoomIdentity);
    }

    private drag() {
        return d3
            .drag()
            .on("start", (d: any) => {
                if (!d3.event.active) {
                    this.simulation.alphaTarget(0.3).restart();
                }
                d.fx = d.x;
                d.fy = d.y;
            })
            .on("drag", (d: any) => {
                d.fx = d3.event.x;
                d.fy = d3.event.y;
            })
            .on("end", () => {
                if (!d3.event.active) {
                    this.simulation.alphaTarget(0);
                }
            });
    }

    private existing = new Map();
    /**
     * Get a random color for a name (or arrow)
     * @param {string} t name
     * @param {boolean} arrow make an arrow
     * @return {string} color or reference to arrow
     */
    private getRandCol(t: string, arrow: boolean = false) {
        if (!this.existing.has(t)) {
            const d3col = d3
                .color(`hsl(${Math.floor(Math.random() * 360)}, 50%, 50%)`)
                .rgb();
            var r = Math.round(d3col.r).toString(16);
            var g = Math.round(d3col.g).toString(16);
            var b = Math.round(d3col.b).toString(16);
            if (d3col.r < 16) {
                r = "0" + r;
            }
            if (d3col.g < 16) {
                g = "0" + g;
            }
            if (d3col.b < 16) {
                b = "0" + b;
            }
            const resCol = r + g + b;

            this.existing.set(t, resCol);

            const markersize = 3;
            this.defs
                .append("svg:marker")
                .attr("id", "marker-" + resCol)
                .attr("refX", 10)
                .attr("refY", markersize / 2)
                .attr("markerWidth", 30)
                .attr("markerHeight", 30)
                .attr("orient", "auto")
                .append("path")
                .attr(
                    "d",
                    `M 0 0 ${markersize} ${markersize /
                        2} 0 ${markersize} ${markersize / 4} ${markersize / 2}`,
                )
                .style("fill", "#" + resCol);
        }
        const color = this.existing.get(t);
        return arrow ? "marker-" + color : "#" + color;
    }
}
