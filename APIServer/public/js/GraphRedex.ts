import * as d3 from "d3";
import { GraphShower } from "./shower/Shower";
import TreeShower from "./shower/TreeShower";
import { downloadFileLink, getit } from "./util";
import { APIDoTermResult, ExampleMeta, TermMeta } from "./_global";
import ForceShower from "./shower/ForceShower";

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

export default class GraphRedex<N extends GRND, E extends GRED> {
    protected curExample: ExampleMeta = null;
    protected shower: GraphShower<N, E>;
    protected expanding: Set<string> = new Set();
    forceNow: boolean;

    constructor(showerConfig: ShowerConfig<N, E> = null) {
        console.log("Booting graph visualiser");
        const config: ShowerConfig<N, E> = showerConfig || {
            nodeOptions: (node) => {
                let ret = [] as Array<ShowerOptionData>;

                if (node.data._stuck === true) {
                    ret.push({
                        name: "Stuck term info",
                        size: 2,
                        icon: "inspect",
                        action: () => {
                            alert("Stuck????" + node.data.term);
                            return false;
                        },
                    });
                } else {
                    ret.push({
                        name: "Expand node",
                        size: 4,
                        icon: "expand",
                        action: () => {
                            if (node.data._limited) {
                                this.render(
                                    this.curExample,
                                    node.data._id,
                                    node.data._id,
                                );
                            } else {
                                this.expandBelow(node.data);
                            }
                            return false;
                        },
                    });
                }

                ret.push({
                    name: "Show term",
                    size: 1,
                    icon: "info",
                    action: () => {
                        alert("Term\n" + node.data.term);
                        return false;
                    },
                });

                if (this.forceNow) {
                    // TODO remove
                    const n = node as any;
                    ret.push({
                        name: "Unpin",
                        size: 1,
                        icon: "unpin",
                        action: () => {
                            n.fx = null;
                            n.fy = null;
                            return false;
                        },
                    });
                }

                return ret;
            },
            nodeMaker: (nodes) => {
                nodes
                    .classed(
                        "start",
                        (d) => d.data._id === this.curExample.baseTerm,
                    )
                    .classed("stuck", (d) => d.data._stuck);

                nodes.on("dblclick", (d) => {
                    d3.event.preventDefault();
                    d3.event.stopPropagation();

                    if (d.data._limited) {
                        d.data._limited = false;
                        this.shower.update();
                        this.render(this.curExample, d.data._id, d.data._id);
                    } else {
                        if (d3.event.shiftKey && d.data._expanded) {
                            this.expandBelow(d.data);
                        } else {
                            this.expandNode(d.data);
                        }
                    }
                });
                nodes.on("mouseover", (d) => {
                    d3.select("#statusSection").html(`
                    <pre style="max-width: 100%;white-space: pre-wrap;">${
                        d.data.term
                    }</pre> <hl>
                <table>
                    <tr><th>Key</th><th>Value</th></tr>
                    ${Object.keys(d.data)
                        .filter((x) => !(x.startsWith("_") || x === "term"))
                        .map(
                            (x) =>
                                `<tr><td>${x}</td><td>${d.data[x]}</td></tr>`,
                        )
                        .join("")}
                    </table>
                    ${
                        d.data._stuck
                            ? "<br>This term has no further reductions."
                            : ""
                    }
                    ${
                        d.data._limited || !d.data._expanded
                            ? `<br>Double click node or single click and choose:
                                <svg width='1em' height='1em'>
                                    <use href='svg.svg#expand'></use>
                                </svg>.`
                            : ""
                    }`);
                });
            },
            nodeUpdate: (nodes) => {
                nodes
                    .classed("expandable", (d) => !d.data._expanded)
                    .classed("expanding", (d) => this.expanding.has(d.data._id))
                    .classed("limited", (d) => d.data._limited);
            },
        };

        this.shower = new ForceShower("#visulisation", config);
        this.forceNow = true;
    }

    init() {
        this.setUpCreateLang();
        this.setupDoReductions();
        this.setupExampleSelector();
        this.setupDoQry();
        this.updateLangs();
        this.setUpShowerBtns();
    }

    /**
     * Render or extend render of the example given
     * @param example the example the query
     * @param start node from which the BFS search is started
     * @param startPos node from whose position the new nodes will be created
     */
    async render(
        example: ExampleMeta,
        start: string = null,
        startPos: string = null,
    ) {
        this.curExample = example;
        this.setupExampleSelector();
        this.shower.setRoot(this.curExample.baseTerm);
        const steps = 300;

        const [data] = await getit("my/example/qry/" + example._key, {
            method: "POST",
            body: `LET nodes = (
                FOR v,e,p IN 0..${steps}
                    OUTBOUND ${start ? `"${start}"` : "@start"} GRAPH @graph
                    OPTIONS {bfs:true,uniqueVertices: 'global'}
                    RETURN DISTINCT MERGE(v,{"_limited": (LENGTH(p.edges) == ${steps} && v._expanded && !v._stuck)}))
        LET edges = (
            FOR a in nodes
                FOR e IN @@edges
                    FILTER  e._from == a._id OR e._to == a._id
                        RETURN DISTINCT e)
        RETURN {nodes,edges}`,
        });

        if (start) {
            this.shower.push(data, startPos);
        } else {
            this.shower.show(data);
        }
    }

    /**
     * Get a list of steps from a given node in the current example that are
     * marked as _real=false. Note that this does not expand the queried node.
     * @param term node to expand
     */
    protected async getNonRealSteps(
        term: TermMeta,
    ): Promise<{ name: string; _to: string; _id: string }[]> {
        return await getit("my/example/qry/" + this.curExample._key, {
            method: "POST",
            body: `
            FOR e IN @@edges
                FILTER  e._from == "${term._id}"
                FILTER  e._real == false
                RETURN DISTINCT {name:e.reduction,_to:e._to,_id:e._id}`,
        });
    }

    /**
     * Send a continueTerm query for the given term and re-render that node and
     * its (new) children.
     */
    protected async expandNode(node: TermMeta) {
        if (!node._expanded && !this.expanding.has(node._id)) {
            this.expanding.add(node._id);
            this.shower.update();
            await getit(`/continueTerm/${this.curExample._key}/${node._key}`, {
                method: "POST",
            });
            this.expanding.delete(node._id);
            node._expanded = true;
            await this.render(this.curExample, node._id, node._id);
        }
    }

    /**
     * Expand non-expanded nodes below the given one
     * @param node node to start from
     * @param depth depth to search for unexpanded
     */
    protected async expandBelow(node: TermMeta, depth: number = 50) {
        if (!node._expanded) {
            this.expandNode(node);
        } else {
            const nodes: { _id: string; _key: string }[] = (
                await getit("my/example/qry/" + this.curExample._key, {
                    method: "POST",
                    body: `FOR v IN 0..${depth}
                    OUTBOUND "${node._id}" GRAPH @graph
                    OPTIONS {bfs:true,uniqueVertices: 'global'}
                    FILTER v._expanded == false
                    RETURN DISTINCT {_key:v._key,_id:v._id}`,
                })
            ).filter((e) => !this.expanding.has(e._id));

            // Mark all selected nodes as expanding
            nodes.forEach((n) => this.expanding.add(n._id));

            this.shower.update(); // and update this in the graph

            // wait till they are all expanded
            await Promise.all(
                nodes.map((n) =>
                    getit(`/continueTerm/${this.curExample._key}/${n._key}`, {
                        method: "POST",
                    })
                        .then(() => {
                            this.expanding.delete(n._id);
                            this.shower.updateNodeData(n._id, (d) => {
                                d._expanded = true;
                            });
                        })
                        .catch((e) => {
                            console.error(e);
                            this.expanding.delete(n._id);
                        }),
                ),
            );

            this.render(this.curExample, node._id, node._id);
        }
    }

    /**
     * Render a graph of the argument if it is an array of length 1
     * whose only element has a nodes and edges key
     */
    renderIfGraph(data: any) {
        // TODO change to promise
        if (data.length === 1) {
            const renderData = data[0];
            const renderKeys = Object.keys(renderData);
            if (renderKeys.includes("nodes") && renderKeys.includes("edges")) {
                if (
                    renderData.nodes
                        .map((x) => x._id)
                        .includes(this.curExample.baseTerm)
                ) {
                    this.shower.setRoot(this.curExample.baseTerm);
                } else {
                    // start not included, don't use BFS TODO: use bfs
                    this.shower.setRoot(null);
                }
                this.shower.show(renderData);
                return true;
            }
        }
        return false;
    }

    private async doTerm(
        lang: string,
        name: string,
        term: string,
    ): Promise<APIDoTermResult> {
        if (name.length === 0) {
            throw "Please supply a name";
        }
        if (lang.length === 0) {
            throw "Please choose a language";
        }
        return await getit(`/doTerm/${lang}/${name}`, {
            method: "POST",
            body: term,
        });
    }

    protected setUpCreateLang() {
        const form = d3.select("#createLanguage").on("submit", () => {
            d3.event.preventDefault();

            const formData = new FormData();
            const a: any = form.select('input[type="file"]').node();
            formData.append("specification", a.files[0]);

            getit("/my/languages/regular", {
                method: "POST",
                body: formData,
            })
                .then(() => {
                    alert("Language created!");
                    this.updateLangs();
                    form.classed("closed", true);
                })
                .catch((e) => {
                    console.log("ERROR", e);
                    alert("something went wrong\n\n" + e);
                });

            return false;
        });
    }

    protected setupDoReductions() {
        const output = document.getElementById("doReductionOutput");
        const form = d3.select("#doReduction");
        const submitBtn = form.select('input[type="submit"]');
        const termArea = form.select("#term");

        form.on("submit", () => {
            submitBtn.attr("disabled", "disabled");

            d3.event.preventDefault();
            output.innerHTML = "Working...";
            const data = termArea.property("value");
            const lang = form.select("#langselector").property("value");
            const name = form.select("#nameselector").property("value");

            this.doTerm(lang, name, data)
                .then((data: APIDoTermResult) => {
                    submitBtn.attr("disabled", null);
                    this.render(data.example);
                    output.textContent = "success";
                    form.classed("closed", true);
                })
                .catch((e) => {
                    submitBtn.attr("disabled", null);
                    output.textContent = JSON.stringify(e, null, 2);
                    if ("e" in data) {
                        output.textContent = data.e;

                        if ("errors" in data) {
                            const errEl = document.createElement("pre");
                            errEl.textContent = data.errors;
                            output.appendChild(errEl);
                        }
                    }
                });
        });
    }

    protected setupDoQry() {
        const output = document.getElementById("doQryOutput");
        const form = d3.select("#createQry").on("submit", () => {
            d3.event.preventDefault();
            const qry = d3.select("#qry").property("value");
            console.log(qry, this.curExample);

            output.textContent = "Wait for it...";
            getit("my/example/qry/" + this.curExample._key, {
                method: "POST",
                body: qry,
            })
                .then((data) => {
                    output.textContent =
                        "Done: See developer console for output\n";
                    console.log("\n\nQuery:\n" + qry);
                    console.log("\n\nResult:");
                    console.log(data);
                    const wasGraph = this.renderIfGraph(data);

                    if (wasGraph) {
                        form.classed("closed", true);
                    } else {
                        output.textContent += JSON.stringify(
                            data,
                            null,
                            2,
                        ).substr(0, 400);
                    }
                })
                .catch((error) => {
                    output.textContent = "ERROR:" + error;
                });
        });
    }

    protected updateLangs() {
        d3.json("/my/languages").then((data: any) => {
            const select = d3.select("#langselector");
            let options = select.selectAll("option").data(data);
            const optionsEnter = options.enter().append("option");
            options.exit().remove();
            options = optionsEnter.merge(options as any);
            options
                .text((d: any) => d.name + " - " + d._key)
                .attr("value", (d: any) => d._key)
                .attr("disabled", null);

            select
                .insert("option", ":first-child")
                .text("Language")
                .attr("value", "")
                .attr("selected", "selected")
                .attr("disabled", "disabled");
        });
    }

    protected setupExampleSelector() {
        d3.json("/my/examples").then((data: any[]) => {
            const select = d3.select("#exampleSelector").on("change", () => {
                const si = select.property("value");
                if (si) {
                    const s = options.filter((d) => d._key === si);
                    const data = s.datum();
                    this.render(data);
                }
            });

            let options = select.selectAll("option").data(data);
            const optionsEnter = options.enter().append("option");
            const curKey = this.curExample ? this.curExample._key : null;
            options.exit().remove();
            options = optionsEnter.merge(options as any);
            options
                .attr("disabled", null)
                .text((d: any) => d.name + " - " + d._key)
                .attr("selected", (d) =>
                    curKey === d._key ? "selected" : null,
                )
                .property("value", (d: any) => d._key);

            select
                .insert("option", ":first-child")
                .text("Example")
                .property("value", "")
                .attr("selected", this.curExample === null ? "selected" : null)
                .attr("disabled", "disabled");
        });
    }

    protected setUpShowerBtns() {
        d3.select("#reheat").on("click", () => {
            this.shower.heatFor(5000);
        });
        d3.select("#rezoom").on("click", () => {
            this.shower.resetZoom();
        });

        d3.select("#getSVG").on("click", () => {
            const statusSection: d3.Selection<any, any, any, any> = d3.select(
                "#statusSection",
            );

            statusSection.html("Preparing your export");
            const { linkEl, url } = downloadFileLink(
                "export.svg",
                this.shower.getSVG(this.svgCSS),
                "image/svg+xml",
            );

            statusSection.html("Download your SVG below");
            statusSection.node().appendChild(linkEl);
            statusSection
                .append("img")
                .attr("src", url)
                .classed("previewExport", true);
            linkEl.click();
        });

        const toggleRenderBtn = d3.select("#toggleRender").on("click", () => {
            this.forceNow = !this.forceNow;
            toggleRenderBtn.text(this.forceNow ? "use tree" : "use force");
            this.shower.swapFor((el, conf, data) => {
                this.shower = this.forceNow
                    ? new ForceShower(el, conf)
                    : new TreeShower(el, conf);
                this.shower.show(data);
                return this.shower;
            });
        });

        toggleRenderBtn.text(this.forceNow ? "use tree" : "use force");
    }

    protected get svgCSS() {
        return `.graph-arrows {
            stroke-width: 2;
            fill: transparent;
        }

        .graph-nodes {
            cursor: move;
            fill: rgb(86, 198, 212);
            stroke: #ffffff;
            stroke-width: 2;
        }

        .graph-nodes .expandable {
            stroke: grey;
        }

        .graph-nodes .limited {
            stroke: pink;
        }



        .graph-nodes .stuck {
            fill: red;
        }
        .graph-nodes .start {
            fill: greenyellow;
        }

        .graph-nodes .expanding {
            fill: orange;
        }

        .graph-nodes .start.stuck {
            fill: greenyellow;
            stroke: red;
        }

        .graph-texts {
            font-size: 5px;
            font-family: "Noto Sans","Courier New",monospace;
        }`;
    }
}
