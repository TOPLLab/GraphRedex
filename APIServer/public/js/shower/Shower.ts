import { randomColor } from "../util";
import * as d3 from "d3";

/// <reference path="./ShowerTypes.d.ts"/>

export type GraphShower<ND extends NodeData, ED extends EdgeData> = Shower<
    ND,
    ED,
    ShowerNode<ND>,
    ShowerEdge<ND, ED>
>;
export default abstract class Shower<
    ND extends NodeData,
    ED extends EdgeData,
    N extends ShowerNode<ND>,
    E extends ShowerEdge<ND, ED>
> {
    // SVG tings
    protected svgRoot: d3.Selection<any, any, any, any>;
    protected width: number = 1000;
    protected height: number = 1000;

    /** zoom handler that will effect the `scene` and `isClose` variables */
    protected zoomHandler: d3.ZoomBehavior<any, any>;
    /** The group that is the immediate child of svg that has zoom transforms */
    protected scene: d3.Selection<any, any, any, any>;
    /** User has zoomed in closely */
    protected isClose: boolean;
    /** A <defs> element in the svg used for defining arrows and shapes */
    protected defs: d3.Selection<any, any, any, any>;

    /** Parts of the visualisation that is rendered */
    protected parts: {
        nodes: d3.Selection<any, N, any, any>;
        arrows: d3.Selection<any, E, any, any>;
        texts: d3.Selection<any, E, any, any>;
    };

    // data
    /** The nodes and the edges that need to be represented,
     * if is assumed that nodes and edges will only be added, never removed
     * unless the entire view is reset.
     */
    protected data: ShowerData<N, E>;
    /** Node map maps node names on the nodes in the tree to find them quickly */
    protected nodeMap: Map<string, N>;

    /** Configuration o the visualisation */
    protected config: ShowerConfigFull<ND, ED, N, E>;

    /**
     *
     * @param svg selector for the svg element to draw in, will be cleared
     * @param showerConfig configuration
     */
    constructor(svg: any, showerConfig: ShowerConfigFull<ND, ED, N, E>) {
        this.svgRoot = d3.select(svg);
        this.svgRoot.html(""); // empty the element, it is now ours
        this.svgRoot
            .attr("width", this.width)
            .attr("height", this.height)
            .attr("viewBox", [
                // place 0,0 in middle
                -this.width / 2,
                -this.height / 2,
                this.width,
                this.height,
            ] as any)
            .call(
                (this.zoomHandler = d3.zoom().on("zoom", () => {
                    this.isClose = d3.event.transform.k > 1;
                    this.scene.attr("transform", d3.event.transform);
                    this.ticked();
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
    }

    /**
     * Sets the root node that will be used for determining depth of nodes
     * If no rootId is given, DFS force will be turned off
     * @param rootId the id of the root node
     */
    public setRoot(rootId: string | null) {
        this.config.rootId = rootId;
    }

    /**
     * @param id id of the node whose data needs to be updated
     * @param f function that gets the new data (will not be called if node not found)
     * @param update true if view needs to be updated after call
     */
    public updateNodeData(
        id: string,
        f: (data: ND) => any,
        update: boolean = false,
    ) {
        if (this.nodeMap.has(id)) {
            f(this.nodeMap.get(id).data);
            if (update) {
                this.update();
            }
        }
    }

    /**
     * Update the node colors...
     */
    public update() {
        const p = this.parts;

        // arrows
        p.arrows = p.arrows.data(this.data.edges);
        const arrowsEnter = p.arrows
            .enter()
            .append("path")
            .attr("stroke", (d) => this.getRandCol(d.data.reduction))
            .attr("stroke-dasharray", (d) => (d.data._real ? null : "5,5"))
            .attr("marker-end", (d) => this.getRandCol(d.data.reduction, true));
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
        if ("nodeUpdate" in this.config) {
            this.config.nodeUpdate(p.nodes);
        }

        // render updated positions
        this.ticked();
    }

    /**
     * Function that is executed on each tick of the force simulation
     * (or if zoom occurred)
     */
    protected ticked() {
        this.parts.arrows
            .attr(
                "d",
                ({ source: s, target: t }) => `M${s.x} ${s.y}L${t.x} ${t.y}`,
            )
            .style("display", (d) =>
                d.source.shown && d.target.shown ? null : "none",
            );

        if (this.isClose) {
            // only render text if close enough
            this.parts.texts
                .transition()
                .style("display", (d) =>
                    d.source.shown && d.target.shown ? null : "none",
                );
            this.parts.texts
                .filter((d) => d.source.shown && d.target.shown)
                .attr("x", (d) => (d.source.x + d.target.x) / 2)
                .attr("y", (d) => (d.source.y + d.target.y) / 2)
                .attr(
                    "transform",
                    ({ source: s, target: t }) => `rotate(
                    ${(180 * Math.atan((s.y - t.y) / (s.x - t.x))) / Math.PI},
                    ${(s.x + t.x) / 2},
                    ${(s.y + t.y) / 2}
                    )`,
                )
                .style("display", null /* = default */);
        } else {
            this.parts.texts.style("display", "none");
        }

        this.parts.nodes.style("display", (d) => (d.shown ? null : "none"));
        this.parts.nodes
            .filter((d) => d.shown)
            .style("display", null)
            .attr("cx", (d) => d.x)
            .attr("cy", (d) => d.y);
    }

    /**
     * @param n node to convert to internal form
     * @param startPos initial position (if x or y not given, the value will be 0)
     */
    protected abstract convertNode(
        n: ND,
        startPos: { x?: number; y?: number },
    ): N;

    /**
     * @param e edge to convert to internal form
     */
    protected abstract convertEdge(e: ED): E;

    /**
     * Reset the view: clear data, remove elements form svg, reset zoom
     */
    public reset() {
        this.data = { nodes: [], edges: [] };
        this.nodeMap = new Map();

        this.parts.arrows.remove();
        this.parts.nodes.remove();
        this.parts.texts.remove();
        this.parts = {
            arrows: this.scene
                .select("g.graph-arrows")
                .selectAll("path")
                .data([]),
            texts: this.scene
                .select("g.graph-texts")
                .selectAll("text")
                .data([]),
            nodes: this.scene
                .select("g.graph-nodes")
                .selectAll("circle")
                .data([]),
        };

        this.resetZoom();

        // Note that defs are kept
    }

    /**
     * Clears the screen and ingests new data to render
     */
    public show(data: InputData, update: boolean = true) {
        this.reset();
        this.push(data, null, update);
    }

    /**
     * Keeps the screen and add nodes to the visualisation
     * @param data New data to be added to the graph
     * @param startPos node id of the initial position of the new nodes
     */
    public push(
        data: InputData,
        startPos: string = null,
        update: boolean = true,
    ) {
        let startNode: N = null;
        if (startPos) {
            startNode = this.nodeMap.get(startPos) || null;
        }

        const newNodes = data.nodes
            .filter((n) => !this.nodeMap.has(n._id))
            .map((n) => this.convertNode(n as ND, startNode));

        this.data.nodes.push(...newNodes);
        newNodes.forEach((n) => this.nodeMap.set(n.id, n));

        // only keep edges between nodes we have
        this.data.edges.push(
            ...data.edges
                .filter(
                    (e) => this.nodeMap.has(e._from) && this.nodeMap.has(e._to),
                )
                .map((e) => this.convertEdge(e as ED)),
        );

        if (update) {
            this.update();
        }
    }

    /** Go back to initial zoom position and scale */
    public resetZoom() {
        this.svgRoot.call(this.zoomHandler.transform, d3.zoomIdentity);
    }

    /** create a drag handler */
    protected drag(): d3.DragBehavior<any, N, any> {
        return d3
            .drag<any, N>()
            .on("start", (d: N) => {
                d.fx = d.x;
                d.fy = d.y;
            })
            .on("drag", (d: N) => {
                d.fx = d3.event.x;
                d.fy = d3.event.y;
            })
            .on("end", (d: N) => {
                d.fx = null;
                d.fy = null;
            });
    }

    /**
     * Heat the visualisation,
     * Whatever that may mean
     */
    public heatFor(_time: number = null) {
        return;
    }

    /**
     * Get the string representation of the graph as SVG.
     * @param css CSS to inject
     */
    public getSVG(css: string = "") {
        // set isClose to true, clone the visualisation and add get the size
        const oldIsClose = this.isClose;
        this.isClose = true;
        this.ticked();
        const clone: SVGElement = this.svgRoot.node().cloneNode(true);
        const { width, height, x, y } = this.scene.node().getBBox();
        this.isClose = oldIsClose;
        this.ticked();

        // set attributes
        clone.setAttribute("width", width);
        clone.setAttribute("height", height);
        clone.setAttribute("xmlns", "http://www.w3.org/2000/svg");

        // give padding of 5 to viewbox
        clone.setAttribute(
            "viewBox",
            `${x - 5} ${y - 5} ${width + 10} ${height + 10}`,
        );

        // reset zoom
        clone.getElementsByTagName("g")[0].removeAttribute("transform");

        if (css.length > 0) {
            const styleTag = document.createElement("style");
            styleTag.innerHTML = css;
            clone.appendChild(styleTag);
        }

        clone.insertBefore(
            document.createComment("Generated by GraphRedex"),
            clone.childNodes[0],
        );

        return clone.outerHTML;
    }

    // Private map for storing colours that have an arrow definition
    private existingColors = new Map<
        string,
        { hex: string; hexFull: string; d3: d3.Color }
    >();
    /**
     * Get a random colour for a name and create an arrow definition for it.
     *
     * example arrow usage
     * arrow.attr("marker-end", d => this.getRandCol(t, true));
     *
     * @param t name to make a colour for
     * @param arrow return marker id if true (else hex colour with #)
     */
    private getRandCol(t: string, arrow: boolean = false): string {
        if (!this.existingColors.has(t)) {
            const randColor = randomColor();

            this.existingColors.set(t, randColor);

            const markersize = 3;
            this.defs
                .append("svg:marker")
                .attr("id", "marker-" + randColor.hex)
                .attr("refX", 8)
                .attr("refY", markersize / 2)
                .attr("markerWidth", markersize * 10)
                .attr("markerHeight", markersize * 10)
                .attr("orient", "auto")
                .append("path")
                .attr(
                    "d",
                    `M 0 0 ${markersize} ${markersize /
                        2} 0 ${markersize} ${markersize / 4} ${markersize / 2}`,
                )
                .style("fill", randColor.hexFull);
        }
        const color = this.existingColors.get(t);
        return arrow ? `url(#marker-${color.hex})` : color.hexFull;
    }
}
