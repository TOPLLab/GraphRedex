/// <reference path="./ShowerTypes.d.ts"/>

import * as d3 from "d3";
import { awaitArray, awaitBoolean, fracToRad, randomColor } from "../util";
import showerDefaultDefs from "./shower-filters.svg";

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
    protected svgRoot: d3.Selection<SVGSVGElement, any, any, any>;
    protected width: number = 1000;
    protected height: number = 1000;

    /** zoom handler that will effect the `scene` and `isClose` variables */
    protected zoomHandler: d3.ZoomBehavior<any, any>;
    /** The group that is the immediate child of svg that has zoom transforms */
    protected scene: d3.Selection<SVGGElement, any, any, any>;
    /** User has zoomed in closely */
    protected isClose: boolean;
    /** A <defs> element in the svg used for defining arrows and shapes */
    protected defs: d3.Selection<SVGDefsElement, any, any, any>;

    /** Parts of the visualization that is rendered */
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
    /** edge map maps edge names on the edges in the tree to find them quickly */
    protected edgeMap: Map<string, E>;

    /** Configuration o the visualization */
    protected config: ShowerConfigFull<ND, ED, N, E>;

    private _selectedNode: N = null;
    protected bubble: d3.Selection<any, any, any, any>;

    /**
     *
     * @param svg selector for the svg element to draw in, will be cleared
     * @param showerConfig configuration
     */
    constructor(svg: any, showerConfig: ShowerConfigFull<ND, ED, N, E>) {
        this.svgRoot = d3.select(svg);
        this.svgRoot.html(""); // empty the element, it is now ours
        this.svgRoot.on("*", null); // remove all listeners
        this.svgRoot
            .attr("focusable", "true") // allow listening for keys
            .attr("tabindex", 0) // allow access for keys
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
                    const t = this.zoomAdapt(d3.event.transform);
                    this.isClose = t.k > 1;
                    this.scene.attr("transform", (t as unknown) as string);
                    this.ticked();
                })),
            );
        this.defs = this.svgRoot.append("svg:defs");
        this.defs.html(showerDefaultDefs);
        this.defs.append("style");

        this.scene = this.svgRoot.append("g");

        this.data = { nodes: [], edges: [] };

        // defaults
        showerConfig.edgeSelected = showerConfig.edgeSelected ?? ((_) => {});
        showerConfig.nodeSelected = showerConfig.nodeSelected ?? ((_) => true);
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
        this.bubble = this.scene
            .insert("g", ".graph-nodes")
            .classed("bubble", true);
    }

    public swapFor(
        constructor: (
            element: SVGSVGElement,
            config: ShowerConfigFull<ND, ED, N, E>,
            data: InputData<ND, ED>,
        ) => Shower<ND, ED, N, E>,
    ) {
        constructor(this.svgRoot.node(), this.config, {
            nodes: this.data.nodes.map((x) => x.data),
            edges: this.data.edges.map((x) => x.data),
        });
    }

    /**
     * Sets the root node that will be used for determining depth of nodes
     * If no rootId is given, DFS force will be turned off
     * @param rootId the id of the root node
     */
    public setRoot(rootId: string | null) {
        this.config.rootId = rootId;
    }

    protected zoomAdapt(t: d3.ZoomTransform): d3.ZoomTransform {
        return t;
    }

    protected selectNode(node: N) {
        this._selectedNode = node;
        if (this.config.nodeSelected && node !== null) {
            if (this.config.nodeSelected(node)) {
                this.showBubble(node);
            }
        }
        this.update();
    }

    public get selectedNode(): N {
        return this._selectedNode;
    }

    protected async showBubble(node: N) {
        if (node === null) {
            this.bubble.datum(null);
            this.ticked();
        }
        if ("nodeOptions" in this.config && this.config.nodeOptions) {
            this.bubble.html('<circle r="20" cx="0" cy="0"></circle>');
            const options = await awaitArray(this.config.nodeOptions(node));
            const s = options.reduce((acc, x) => acc + (x.size || 1), 0);
            let prevRad = -fracToRad((options[0].size || 1) / s) / 2;
            let correctTarget = prevRad;
            for (const option of options) {
                const target = fracToRad((option.size || 1) / s);
                correctTarget += target;
                const iconTarget = prevRad + target / 2;
                const r = 20,
                    largearcflag = target > Math.PI, // true if > 180 deg
                    xs = r * Math.cos(prevRad),
                    ys = r * Math.sin(prevRad),
                    xe = r * Math.cos(correctTarget),
                    ye = r * Math.sin(correctTarget);

                this.bubble
                    .append("path")
                    .attr(
                        "d",
                        options.length === 1
                            ? `M${xs} ${ys} A${r},${r} 0 ${
                                  largearcflag ? 1 : 0
                              } 1 ${xe} ${ye}`
                            : `M0 0 L${xs} ${ys} A${r},${r} 0 ${
                                  largearcflag ? 1 : 0
                              } 1 ${xe} ${ye}L0 0`,
                    )
                    .on("click", async () => {
                        if (!(await awaitBoolean(option.action()))) {
                            this.bubble.datum(null);
                        }
                    })
                    .append("title")
                    .text(option.name);

                const iconx = r * 0.75 * Math.cos(iconTarget),
                    icony = r * 0.75 * Math.sin(iconTarget),
                    icondim = r / 5;
                this.bubble
                    .append("use")
                    .attr("height", icondim)
                    .attr("width", icondim)
                    .attr("href", `svg.svg#${option.icon || "cogs"}`)
                    .attr("x", iconx - icondim / 2)
                    .attr("y", icony - icondim / 2);
                prevRad = correctTarget;
            }

            this.bubble.datum(node);
            this.ticked();
        }
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

        if ("edgeSelected" in this.config && this.config.edgeSelected) {
            arrowsEnter.on("click", (d) => {
                this.config.edgeSelected(d);
            });
        }
        p.arrows = arrowsEnter.merge(p.arrows);
        p.arrows.exit().remove();

        // texts
        p.texts = p.texts.data(this.data.edges);
        const textEnter = p.texts
            .enter()
            .append("text")
            .attr("text-anchor", "middle")
            .attr("fill", (d) => this.getRandCol(d.data.reduction))
            .html((d) =>
                d.data.reduction === false
                    ? ""
                    : "<tspan dy='-5'>" + d.data.reduction + "</tspan>",
            );
        if ("edgeSelected" in this.config && this.config.edgeSelected) {
            textEnter.on("click", (d) => {
                this.config.edgeSelected(d);
            });
        }
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

        if ("nodeOptions" in this.config && this.config.nodeOptions) {
            nodeEnter.on("click", (d) => {
                d3.event.preventDefault();
                d3.event.stopPropagation();
                this.selectNode(d);
            });
        } else {
            if ("nodeSelected" in this.config && this.config.nodeSelected) {
                nodeEnter.on("click", (d) => {
                    this.config.nodeSelected(d);
                });
            }
        }
        p.nodes = nodeEnter.merge(p.nodes);
        p.nodes.exit().remove();
        if ("nodeUpdate" in this.config) {
            this.config.nodeUpdate(p.nodes);
        }

        this.defs.select("style").text(this.config.css());

        // render updated positions
        this.ticked();
    }

    /**
     * Function that is executed on each tick of the force simulation
     * (or if zoom occurred)
     */
    protected ticked() {
        this.bubble
            .style("display", (d) => (d ? null : "none"))
            .attr("transform", (d) => (d ? `translate(${d.x},${d.y})` : ""));
        this.parts.arrows
            .attr(
                "d",
                ({ source: s, target: t }) =>
                    `M${s.x} ${s.y}v-1v2v-1h1h-2h1L${t.x} ${t.y}`,
                // add a little plus so chrome gives te line a width and height
            )
            .style("display", (d) =>
                d.source.shown && d.target.shown ? null : "none",
            );

        if (this.isClose) {
            // only render text if close enough
            const calcAngle = (s, t) => {
                const tanValue = (s.y - t.y) / (s.x - t.x);
                if (isNaN(tanValue)) {
                    return 0;
                } else {
                    return (180 * Math.atan(tanValue)) / Math.PI;
                }
            };
            this.parts.texts.style("display", (d) =>
                d.source.shown && d.target.shown ? null : "none",
            );
            this.parts.texts
                .filter((d) => d.source.shown && d.target.shown)
                .attr("x", (d) => (d.source.x + d.target.x) / 2)
                .attr("y", (d) => (d.source.y + d.target.y) / 2)
                .attr(
                    "transform",
                    ({ source: s, target: t }) =>
                        `rotate(${calcAngle(s, t)},${(s.x + t.x) / 2},${
                            (s.y + t.y) / 2
                        })`,
                );
        } else {
            this.parts.texts.style("display", "none");
        }

        this.parts.nodes.style("display", (d) => (d.shown ? null : "none"));
        this.parts.nodes
            .filter((d) => d.shown)
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
        this.edgeMap = new Map();

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

        this.selectNode(null); // hide bubble
        // Note that defs are kept
    }

    /**
     * Clears the screen and ingests new data to render
     */
    public show(data: InputData<ND, ED>, update: boolean = true) {
        this.reset();
        this.push(data, null, update);
    }

    /**
     * Keeps the screen and add nodes to the visualization
     * @param data New data to be added to the graph
     * @param startPos node id of the initial position of the new nodes
     */
    public push(
        data: InputData<ND, ED>,
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
        const newEdges = data.edges
            .filter((e) => this.nodeMap.has(e._from) && this.nodeMap.has(e._to))
            .filter((e) => !this.edgeMap.has(e._id))
            .map((e) => this.convertEdge(e as ED));
        this.data.edges.push(...newEdges);

        newEdges.forEach((e) => this.edgeMap.set(e.data._id, e));

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
        return d3.drag<any, N>();
    }

    /**
     * Heat the visualization,
     * Whatever that may mean
     */
    public heatFor(_time: number = null) {
        return;
    }

    /**
     * Get the string representation of the graph as SVG.
     * @param css CSS to inject
     */
    public getSVG() {
        // set isClose to true, clone the visualization and add get the size
        const oldIsClose = this.isClose;
        this.isClose = true;
        this.ticked();
        const clone: SVGSVGElement = this.svgRoot
            .node()
            .cloneNode(true) as SVGSVGElement;
        const { width, height, x, y } = this.scene.node().getBBox();
        this.isClose = oldIsClose;
        this.ticked();

        // set attributes
        clone.setAttribute("width", width.toString(10));
        clone.setAttribute("height", height.toString(10));
        clone.setAttribute("xmlns", "http://www.w3.org/2000/svg");

        // give padding of 5 to view box
        clone.setAttribute(
            "viewBox",
            `${x - 5} ${y - 5} ${width + 10} ${height + 10}`,
        );

        // reset zoom
        clone.getElementsByTagName("g")[0].removeAttribute("transform");

        clone.querySelector("g>g.bubble").remove();

        clone.insertBefore(
            document.createComment("Generated by GraphRedex"),
            clone.childNodes[0],
        );

        return clone.outerHTML;
    }

    // Private map for storing colors that have an arrow definition
    private existingColors = new Map<
        string,
        { hex: string; hexFull: string; d3: d3.Color }
    >();
    /**
     * Get a random color for a name and create an arrow definition for it.
     *
     * example arrow usage
     * arrow.attr("marker-end", d => this.getRandCol(t, true));
     *
     * @param t name to make a color for
     * @param arrow return marker id if true (else hex color with #)
     */
    private getRandCol(t: string | false, arrow: boolean = false): string {
        if (t === false) {
            return null;
        }
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
                    `M 0 0 ${markersize} ${markersize / 2} 0 ${markersize} ${
                        markersize / 4
                    } ${markersize / 2}`,
                )
                .style("fill", randColor.hexFull);
        }
        const color = this.existingColors.get(t);
        return arrow ? `url(#marker-${color.hex})` : color.hexFull;
    }
}
