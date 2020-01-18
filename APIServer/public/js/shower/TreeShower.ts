import * as d3 from "d3";
import { mLast } from "../util";
import Shower from "./Shower";

const enum Direction {
    Forward,
    Backward,
}

interface ShowerNodeTree<ND extends NodeData, ED extends EdgeData>
    extends ShowerNode<ND> {
    forwardArrows: Map<string, ShowerEdgeTree<ND, ED>>;
    backArrows: Map<string, ShowerEdgeTree<ND, ED>>;
    arrows: (direction: Direction) => Map<string, ShowerEdgeTree<ND, ED>>;
}

interface ShowerEdgeTree<ND extends NodeData, ED extends EdgeData> {
    source: ShowerNodeTree<ND, ED>;
    target: ShowerNodeTree<ND, ED>;
    data: ED;
}

/**
 * The tree shower is a non-force directed representation of the graph.
 * it renders a tree in both directions form a selected node
 *
 * TODO:
 *   - click to select
 *   - nicer handling of events so that we can switch between representations
 *   - doc
 */
export default class TreeShower<
    ND extends NodeData,
    ED extends EdgeData
> extends Shower<ND, ED, ShowerNodeTree<ND, ED>, ShowerEdgeTree<ND, ED>> {
    constructor(
        svg: any,
        showerConfig: ShowerConfigFull<
            ND,
            ED,
            ShowerNodeTree<ND, ED>,
            ShowerEdgeTree<ND, ED>
        >,
    ) {
        super(svg, showerConfig);
        console.log("listening for keys", this.zoomHandler);
        this.zoomHandler.scaleExtent([1.1, 8]);
        this.svgRoot.on("zoom", () => {
            var t = d3.event.transform;
            t.x = d3.max([d3.min([t.x, -(1 - t.k) * 500]), (1 - t.k) * 500]);
            this.isClose = t.k > 1;
            this.scene.attr("transform", t);
            this.ticked();
        });
        this.svgRoot.on("keydown", () => {
            const e = d3.event as KeyboardEvent;
            if (e.altKey || e.metaKey || e.ctrlKey) {
                return;
            } // don't mess with shortcuts
            if (e.key.startsWith("Arrow")) {
                e.preventDefault();
                this.handleArrow(e.key.substr(5));
            }
        });
    }

    private selectedIndex = 0;
    private selectedNode: ShowerNodeTree<ND, ED> = null;
    private selectedDirection: Direction = Direction.Forward;

    protected zoomAdapt(t: d3.ZoomTransform) {
        return d3.zoomIdentity
            .scale(Math.max(t.k, 2)) //Limit zoom
            .translate(0, -50); // move up to accomodate infobar
    }

    private get selectedArrow() {
        const selectedSet = this.selectedNode.arrows(this.selectedDirection);
        if (selectedSet.size === 0) {
            throw "Could not get selected arrow";
        }
        return [...selectedSet.values()][this.selectedIndex % selectedSet.size];
    }

    private selectNode(node: ShowerNodeTree<ND, ED>) {
        this.selectedNode = node;
        this.selectedIndex = 0;
        if (this.config.nodeSelected) {
            if (this.config.nodeSelected(this.selectedNode)) {
                this.showBubble(this.selectedNode);
            }
        }
    }

    private handleArrow(direction: string) {
        const oldSelecteArrow = this.selectedArrow;
        switch (direction) {
            case "Left":
                if (this.selectedDirection === Direction.Backward) {
                    this.selectNode(this.selectedArrow.source);
                } else {
                    this.selectedDirection = Direction.Backward;
                    this.selectedIndex = 0;
                }
                break;
            case "Right":
                if (this.selectedDirection === Direction.Forward) {
                    this.selectNode(this.selectedArrow.target);
                    this.selectedIndex = 0;
                } else {
                    this.selectedDirection = Direction.Forward;
                }
                this.selectedIndex = 0;
                break;
            case "Up":
                this.selectedIndex--;
                break;
            case "Down":
                this.selectedIndex++;
                break;
            default:
                return; // no further action
        }

        // validate if new state is possible
        let possible = this.selectedNode.arrows(this.selectedDirection).size;

        if (possible === 0) {
            // bad direction, should ignore
            if (this.selectedDirection === Direction.Forward) {
                this.selectedDirection = Direction.Backward;
            } else {
                this.selectedDirection = Direction.Forward;
            }
            possible = this.selectedNode.arrows(this.selectedDirection).size;
        }

        if (possible === 0) {
            throw "No arrows";
        }
        this.selectedIndex = (2 * possible + this.selectedIndex) % possible;
        console.log(this.config.rootId);
        if (
            this.selectedArrow !== oldSelecteArrow &&
            this.config.edgeSelected
        ) {
            this.config.edgeSelected(this.selectedArrow);
        }
        this.update();
    }

    private expand(
        maxDepth: number,
        xDelta: number,
        getArrows: (
            n: ShowerNodeTree<ND, ED>,
        ) => Map<any, ShowerEdgeTree<ND, ED>>,
        getArrowTarget: (
            edge: ShowerEdgeTree<ND, ED>,
        ) => ShowerNodeTree<ND, ED>,
    ): Set<string> {
        const accessibleSet = new Set<string>();

        const doExpand = (
            node: ShowerNodeTree<ND, ED>,
            accessible: boolean,
            depth: number,
            ymin: number,
            ymax: number,
            x: number,
        ) => {
            if (depth >= maxDepth) {
                return;
            }
            const arrows = [...getArrows(node).values()];
            const num = arrows.length;
            if (num > 0) {
                const spanPerNode = (ymax - ymin) / num;
                for (const [arrow, i] of mLast(arrows)) {
                    const target = getArrowTarget(arrow);
                    const edgeAccessible =
                        accessible ||
                        arrow.data._id === this.selectedArrow.data._id;
                    if (edgeAccessible) {
                        accessibleSet.add(arrow.data._id);
                        accessibleSet.add(target.data._id);
                    }
                    doExpand(
                        target,
                        edgeAccessible,
                        depth + 1,
                        ymin + spanPerNode * i,
                        ymin + spanPerNode * (i + 1),
                        x + xDelta,
                    );
                }
                for (const [arrow, i] of mLast(arrows)) {
                    const target = getArrowTarget(arrow);
                    target.shown = true;
                    target.y = ymin + spanPerNode * i + spanPerNode / 2;
                    target.x = x;
                }
            }
        };
        doExpand(this.selectedNode, false, /* depth */ 1, -200, 200, xDelta);
        return accessibleSet;
    }

    public update() {
        if (this.selectedNode === null) {
            this.selectedNode = this.nodeMap.get(this.config.rootId);
            this.selectedIndex = 0;
        }
        this.data.nodes.forEach((e) => (e.shown = false));
        const accessibleSets = {};
        accessibleSets[Direction.Forward] = this.expand(
            7,
            100,
            (n) => n.forwardArrows,
            (e) => e.target,
        );
        accessibleSets[Direction.Backward] = this.expand(
            7,
            -100,
            (n) => n.backArrows,
            (e) => e.source,
        );
        const accessibleSet = accessibleSets[this.selectedDirection];
        this.selectedNode.x = 0;
        this.selectedNode.y = 0;
        this.selectedNode.shown = true;

        // Redefine and restart simulation
        super.update();
        this.parts.arrows
            .classed("accessible", (d) => accessibleSet.has(d.data._id))
            .classed("inaccessible", (d) => !accessibleSet.has(d.data._id));

        const selArrId = this.selectedArrow
            ? this.selectedArrow.data._id
            : false;
        this.parts.arrows.classed("selected", (d) => selArrId === d.data._id);

        this.parts.nodes
            .classed(
                "selected",
                (d) => d.data._id === this.selectedNode.data._id,
            )
            .classed("accessible", (d) => accessibleSet.has(d.data._id))
            .classed("inaccessible", (d) => !accessibleSet.has(d.data._id));
    }

    protected convertNode(
        n: ND,
        startPos: { x?: number; y?: number } = null,
    ): ShowerNodeTree<ND, ED> {
        const o = {
            id: n._id,
            data: n,
            x: startPos && startPos.x ? startPos.x : 11000,
            y: startPos && startPos.y ? startPos.y : 0,
            backArrows: new Map(),
            forwardArrows: new Map(),
            shown: false,
            arrows: (dir: Direction) => {
                return dir === Direction.Forward
                    ? o.forwardArrows
                    : o.backArrows;
            },
        };
        return o;
    }

    protected convertEdge(e: ED): ShowerEdgeTree<ND, ED> {
        return {
            source: this.nodeMap.get(e._from),
            target: this.nodeMap.get(e._to),
            data: e,
        };
    }

    public show(data: InputData<ND, ED>, update: boolean = true) {
        super.show(data, false);
        this.selectedNode = this.nodeMap.get(this.config.rootId);
        if (update) {
            this.update();
        }
    }

    public push(
        data: InputData<ND, ED>,
        startPos: string = null,
        update: boolean = true,
    ) {
        super.push(data, startPos, update);
        for (const e of this.data.edges) {
            this.nodeMap.get(e.source.id).forwardArrows.set(e.data._id, e);
            this.nodeMap.get(e.target.id).backArrows.set(e.data._id, e);
        }
        if (update) {
            this.update();
        }
    }

    // no dragging
    protected drag(): d3.DragBehavior<any, any, any> {
        return (() => {
            return null;
        }) as any;
    }
}
