import * as d3 from "d3";
import ForceShower from "./shower/ForceShower";
import { GraphShower } from "./shower/Shower";
import TreeShower from "./shower/TreeShower";
import termDiff, { doubleTermDiff, TermDiffMarkup } from "./termDiff";
import { downloadFileLink, genHighlightId, getit } from "./util";
import { APIDoTermResult, ExampleMeta, TermMeta } from "./_global";

interface RulesDef {
    [key: string]: string;
}

interface GRND extends NodeData {
    _id: string;
    _key: string;
    term: string;
    _stuck: boolean;
    _limited?: boolean;
    _expanded: boolean;
    _pict?: string;
    _formatted?: string;
}

interface GRED extends EdgeData {
    _id: string;
    _from: string;
    _to: string;
    reduction: string;
    _real: boolean;
}

interface Hightlights {
    nodes: Set<string>; // _id of node
    edges: Set<string>; // _id of edge
    name: string;
    id: string; // string that can be used as a classname
}

export default class GraphRedex<N extends GRND, E extends GRED> {
    forceNow: boolean;
    protected shower: GraphShower<N, E>;
    protected expanding: Set<string> = new Set();
    private highlighted: Set<Hightlights> = new Set();

    constructor(showerConfig: ShowerConfig<N, E> = null) {
        console.log("Booting graph visualizer");
        const config: ShowerConfig<N, E> = showerConfig || {
            nodeSelected: (_n) => {
                return true;
            },
            edgeSelected: (e) => {
                d3.select("#ruleInfo").text(e.data.reduction);

                const f: TermDiffMarkup = (x, d) => {
                    let el = "span";
                    let a = document.createElement("a");
                    a.innerHTML = x;
                    if (a.textContent.length > 20) {
                        el = "div";
                    }
                    return `<${el} class="${d ? "diff" : "same"}">${x}</${el}>`;
                };
                const diff = doubleTermDiff(
                    e.source.data.term,
                    e.target.data.term,
                    f,
                );

                ["#diffFrom", "#diffTo"]
                    .map((x) => d3.select(x))
                    .forEach((el, index) => {
                        el.html(diff[index]);

                        el.select<HTMLElement>(".diff")
                            .node()
                            .scrollIntoView({ block: "center" });
                    });

                this.curLang.then((cl) => {
                    const svg = cl.rules[e.data.reduction];
                    if (svg) {
                        d3.select("#ruleInfo").html(svg);
                    }
                });
            },
            css: () => {
                return this.svgCSS;
            },
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

                let prevURL = null;
                nodes.on("mouseover", (d) => {
                    // Get the diffed term
                    let renderedTerm =
                        this.shower.bubbleNode === null
                            ? this.getRepr(d.data)
                            : termDiff(
                                  this.getRepr(d.data),
                                  this.getRepr(this.shower.bubbleNode.data),
                              );

                    if ("_pict" in d.data) {
                        if (prevURL) {
                            URL.revokeObjectURL(prevURL);
                        }
                        prevURL = URL.createObjectURL(
                            new Blob([d.data._pict], { type: "image/svg+xml" }),
                        );
                        const img = document.createElement("img");
                        img.src = prevURL;
                        img.alt = d.data.term;
                        img.style.maxWidth = "100%";
                        img.style.userSelect = "all";
                        renderedTerm = img.outerHTML + "<hr/>" + renderedTerm;
                    }

                    d3.select("#statusSection").html(`
                    <table class="pure-table pure-table-horizontal stretch"><thead>
                    <tr><th>Key</th><th>Value</th></tr></thead><tbody>
                    ${Object.keys(d.data)
                        .filter(
                            (x) =>
                                !(
                                    x.startsWith("_") ||
                                    x === "repr" ||
                                    x === "term"
                                ),
                        )
                        .map(
                            (x) =>
                                `<tr><td>${x}</td><td>${d.data[x]}</td></tr>`,
                        )
                        .join("")}
                    ${[...this.highlighted]
                        .filter((h) => h.nodes.has(d.data._id))
                        .map((h) => `<tr><td>in</td><td>${h.name}</td></tr>`)
                        .join("")}
                    </tbody></table>
                    <hr>
                    <pre style="max-width: 100%;white-space: pre-wrap;">${renderedTerm}</pre>
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

                for (const highlight of this.highlighted) {
                    nodes.classed(`highlight-${highlight.id}`, (d) =>
                        highlight.nodes.has(d.data._id),
                    );
                }
            },
        };

        this.shower = new ForceShower("#visulisation", config);
        this.forceNow = true;
    }

    private getRepr(nd: GRND) {
        if ("_formatted" in nd) {
            return nd._formatted;
        }
        return nd.term;
    }

    protected _curExample: ExampleMeta = null;
    protected _curLang: Promise<{ rules: RulesDef }> = null;

    get curExample() {
        return this._curExample;
    }

    get curLang() {
        return this._curLang;
    }

    set curExample(example: ExampleMeta | null) {
        this._curExample = example;

        this._curLang = getit("/my/language/" + example.lang, {
            method: "GET",
        });

        this.setupExampleSelector();
    }

    protected async doQry(
        qry: string,
        example: ExampleMeta = null,
        focused: NodeData = null,
    ): Promise<any[]> {
        const ex = example ?? this.curExample;
        const focus = focused ?? this.shower.bubbleNode?.data ?? null;
        if (typeof ex?._key !== "string") {
            throw "No example selected!";
        }
        console.log(focus);
        return await getit(`my/example/qry/${ex._key}`, {
            method: "POST",
            headers: new Headers([["Content-Type", "application/json"]]),
            body: JSON.stringify({ qry, focus: focus?._id ?? null }),
        });
    }

    protected get svgCSS() {
        return (
            `.graph-arrows {
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

.graph-nodes .highlighted {
    stroke: red !important;
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
}` +
            [...this.highlighted]
                .map(
                    (h, i) =>
                        // TODO: better
                        `.highlight-${h.id}{stroke:hsl(${17 +
                            i * 37}, 100%, 50%);}`,
                )
                .join("\n")
        );
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
        this.shower.setRoot(this.curExample.baseTerm);
        const steps = 300;

        const [data] = await this.doQry(`LET nodes = (
                FOR v,e,p IN 0..${steps}
                    OUTBOUND ${start ? `"${start}"` : "@start"} GRAPH @graph
                    OPTIONS {bfs:true,uniqueVertices: 'global'}
                    RETURN DISTINCT MERGE(v,{"_limited": (LENGTH(p.edges) == ${steps} && v._expanded && !v._stuck)}))
        LET edges = (
            FOR a in nodes
                FOR e IN @@edges
                    FILTER  e._from == a._id OR e._to == a._id
                        RETURN DISTINCT e)
        RETURN {nodes,edges}`);

        if (start) {
            this.shower.push(data, startPos);
        } else {
            this.shower.show(data);
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
        } else {
            throw "Rendering nodes is not implemented yet";
        }
        return false;
    }

    /**
     * Render a graph of the argument if it is an array of length 1
     * whose only element has a nodes and edges key
     */
    highlightIfGraph(data: any[]) {
        // TODO change to promise
        if (data.length === 1) {
            const renderData = data[0];
            const renderKeys = Object.keys(renderData);
            if (renderKeys.includes("nodes") && renderKeys.includes("edges")) {
                this.highlighted.add({
                    edges: new Set(
                        renderData.edges.map((x: EdgeData) => x._id),
                    ),
                    nodes: new Set(
                        renderData.nodes.map((x: NodeData) => x._id),
                    ),
                    name: "lol",
                    id: genHighlightId(),
                });
                this.updateHighlightList();
                this.shower.update();
                return true;
            }
        } else {
            if (data.every((x) => typeof x === "object" && "_id" in x)) {
                data = data.map((x) => x._id);
            }

            const d = data as Array<string>;

            // TODO add prefix to exampledata
            const prefix = this.curExample.baseTerm.split("/")[0];
            this.highlighted.add({
                edges: new Set(
                    d.filter((n) => n.startsWith(`${prefix}-reductions/`)),
                ),
                nodes: new Set(d.filter((n) => n.startsWith(`${prefix}/`))),
                name: "lol",
                id: genHighlightId(),
            });

            this.updateHighlightList();
            this.shower.update();
            return true;
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
        return await this.doQry(`
            FOR e IN @@edges
                FILTER  e._from == "${term._id}"
                FILTER  e._real == false
                RETURN DISTINCT {name:e.reduction,_to:e._to,_id:e._id}`);
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
                await this.doQry(`FOR v IN 0..${depth}
                    OUTBOUND "${node._id}" GRAPH @graph
                    OPTIONS {bfs:true,uniqueVertices: 'global'}
                    FILTER v._expanded == false
                    RETURN DISTINCT {_key:v._key,_id:v._id}`)
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

    protected updateHighlightList() {
        const arr = d3.select("#activeHighlights > ul");
        let hls = arr.selectAll("li").data([...this.highlighted]);
        const highlightsEnter = hls.enter().append("li");
        hls.exit().remove();
        hls = highlightsEnter.merge(hls as any);
        hls.text(
            (d: Hightlights) =>
                d.name + " - " + d.id + `(${d.nodes.size},${d.edges.size})`,
        );

        //TODO: add colour changing
        hls.on("dblclick", (d) => {
            let newName = window.prompt("Name", d.name);
            if (newName) {
                d.name = newName;
            }
            this.updateHighlightList();
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
        const form = d3.select("#createQry");
        const submitBtn = form.selectAll('input[type="submit"]');
        console.log("yay", submitBtn);
        submitBtn.on("click", () => {
            d3.event.preventDefault();
            d3.event.stopPropagation();
            const qryType: string = d3.event.target.dataset.type;
            const qry = d3.select("#qry").property("value");

            output.textContent = "Wait for it...";
            this.doQry(qry)
                .then((data) => {
                    output.textContent =
                        "Done: See developer console for output\n";
                    console.log("\n\nQuery:\n" + qry);
                    console.log("\n\nResult:");
                    console.log(data);

                    const succesfullGraphAction = {
                        highlight: (d: any[]) => {
                            this.highlightIfGraph(d);
                        },
                        find: (d: any[]) => {
                            this.renderIfGraph(d);
                        },
                    }[qryType](data);

                    if (succesfullGraphAction) {
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
                d3.event.preventDefault();

                const si = select.property("value");
                if (si) {
                    const s = options.filter((d) => d._key === si);
                    const data = s.datum();
                    window.setTimeout(() => {
                        this.render(data);
                    }, 0);
                }
            });

            let options = select.selectAll("option").data(data);
            const optionsEnter = options.enter().append("option");
            const curKey = this.curExample ? this.curExample._key : null;
            options.exit().remove();
            options = optionsEnter.merge(options as any);
            options
                .attr("disabled", null)
                .text(
                    (d: any) =>
                        d.name +
                        " - " +
                        d._key +
                        (curKey === d._key ? " (current)" : ""),
                )
                .property("selected", (d) =>
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

    private reset() {
        this.shower.reset();
        this.highlighted.clear();
        this.updateHighlightList();
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
                this.shower.getSVG(),
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

        d3.select("#deleteCurExample").on("click", () => {
            if (confirm("Do you want to delete\n" + this.curExample.name)) {
                getit("my/example/" + this.curExample._key, {
                    method: "DELETE",
                })
                    .then(() => {
                        alert("removed");
                    })
                    .catch((e) => alert(e));
                this.curExample = null;
                this.reset();
            }
        });

        const toggleRenderBtn = d3.select("#toggleRender");
        const doRenderBtn = () => {
            toggleRenderBtn.html("");
            toggleRenderBtn
                .attr(
                    "title",
                    `Switch to ${
                        this.forceNow ? "tree" : "force"
                    }-based rendering`,
                )
                .append("svg")
                .append("use")
                .attr("href", `svg.svg#${this.forceNow ? "physics" : "tree"}`);
        };

        toggleRenderBtn.on("click", () => {
            this.forceNow = !this.forceNow;
            doRenderBtn();
            this.shower.swapFor((el, conf, data) => {
                this.shower = this.forceNow
                    ? new ForceShower(el, conf)
                    : new TreeShower(el, conf);
                this.shower.show(data);
                return this.shower;
            });
        });
        doRenderBtn();
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
}
