import * as d3 from "d3";

import { APIDoTermResult, ExampleMeta, TermMeta } from "./_global";
/// <reference path="./static-files.d.ts"/>
import * as graphRedexGraphCSS from "./GraphRedex-graph.less";
import predefinedQueries from "./predefinedQueries.cnf";
import ForceShower from "./shower/ForceShower";
import { GraphShower } from "./shower/Shower";
import TreeShower from "./shower/TreeShower";
import { doubleTermDiff } from "./termDiff";
import {
    downloadFileLink,
    genHighlightId,
    getit,
    mkRandomColorGenerator,
    termDiffMaker,
} from "./util";

interface RulesDef {
    [key: string]: string;
}

interface Language {
    rules: RulesDef;
    query?: { name: string; query: string }[];
    _key: string;
}

type Awaitable<D> = D | Promise<D>;

interface GRND extends NodeData {
    _id: string;
    _key: string;
    term: Awaitable<string>;
    _stuck?: boolean;
    _limited?: boolean;
    _expanded: boolean;
    _pict?: Awaitable<string>;
    _formatted?: Awaitable<string>;
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
    colour: string;
}

export default class GraphRedex<N extends GRND, E extends GRED> {
    forceNow: boolean;
    protected shower: GraphShower<N, E>;
    protected expanding: Set<string> = new Set();
    private highlighted: Set<Hightlights> = new Set();
    private plugin: any;

    constructor(showerConfig: ShowerConfig<N, E> = null) {
        console.log("Booting graph visualizer");
        const config: ShowerConfig<N, E> = showerConfig || {
            nodeSelected: (n: ShowerNode<N>) => {
                if (!this.forceNow) {
                    // expand till depth 5 if in tree mode
                    this.expandBelow(n.data, 5);
                    this.fillSidebar(n.data);
                }
                return true;
            },
            edgeSelected: async (e) => {
                d3.select("#treeInfoBar").classed("closed", false);
                d3.select("#ruleInfo").text(e.data.reduction);

                const diff = doubleTermDiff(
                    await e.source.data.term,
                    await e.target.data.term,
                    termDiffMaker,
                );

                ["#diffFrom", "#diffTo"]
                    .map((x) => d3.select(x))
                    .forEach((el, index) => {
                        el.html(diff[index].innerHTML);

                        el.select<HTMLElement>(".diff")
                            .node()
                            .scrollIntoView({ block: "center" });
                    });

                this.curLang.then((cl) => {
                    if (cl.rules ?? false) {
                        const svg = cl.rules[e.data.reduction];
                        if (svg) {
                            d3.select("#ruleInfo").html(svg);
                        }
                    }
                });
            },
            css: () => {
                return this.svgCSS;
            },
            nodeOptions: (node) => {
                let ret = [] as Array<ShowerOptionData>;

                if ((node.data._stuck ?? false) === false) {
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
                    name: "Show term for editing",
                    size: 1,
                    icon: "edit",
                    action: () => {
                        (async () => {
                            d3.select("#term").property(
                                "value",
                                await node.data.term,
                            );
                            (d3
                                .select("#term")
                                .node() as HTMLTextAreaElement).dispatchEvent(
                                new Event("change"),
                            );
                            (window as any).toggle("doReduction");
                        })();
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
                    .classed("stuck", (d) => d.data._stuck ?? false);

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
                    this.fillSidebar(d.data);
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

        this.shower = new ForceShower<N, E>("#visulisation", config);
        this.forceNow = true;
    }

    private async getRepr(nd: GRND): Promise<string> {
        const formatted = (await nd._formatted) ?? null;
        if (formatted !== null) {
            return formatted;
        }
        return await nd.term;
    }

    private sidebarCellHTML(v: any): string {
        if (Array.isArray(v)) {
            return (
                "<table>" +
                v
                    .map(
                        (cv) => `<tr><td>${this.sidebarCellHTML(cv)}</td></tr>`,
                    )
                    .join("") +
                "</table>"
            );
        }

        return v.toString();
    }

    private prevPictSVGURL: string = null;
    private async fillSidebar(nd: N) {
        d3.select("#statusSection").html("loading");
        // Get the diffed term
        let renderedTerm = await this.getRepr(nd);

        const pictSVG = (await nd._pict) ?? null;
        if (pictSVG !== null) {
            if (this.prevPictSVGURL) {
                URL.revokeObjectURL(this.prevPictSVGURL);
            }
            this.prevPictSVGURL = URL.createObjectURL(
                new Blob([pictSVG], {
                    type: "image/svg+xml",
                }),
            );
            const img = document.createElement("img");
            img.src = this.prevPictSVGURL;
            img.alt = await nd.term;
            img.style.maxWidth = "100%";
            img.style.userSelect = "all";
            renderedTerm = img.outerHTML + "<hr/>" + renderedTerm;
        }

        d3.select("#statusSection").html(`
                    <table class="pure-table pure-table-horizontal stretch"><thead>
                    <tr><th>Key</th><th>Value</th></tr></thead><tbody>
                    ${Object.getOwnPropertyNames(nd)
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
                                `<tr><td>${x}</td><td>${this.sidebarCellHTML(
                                    nd[x],
                                )}</td></tr>`,
                        )
                        .join("")}
                    ${[...this.highlighted]
                        .filter((h) => h.nodes.has(nd._id))
                        .map((h) => `<tr><td>in</td><td>${h.name}</td></tr>`)
                        .join("")}
                    </tbody></table>
                    <hr>
                    <pre style="max-width: 100%;white-space: pre-wrap;">${renderedTerm}</pre>
                    ${
                        nd._stuck ?? false
                            ? "<br>This term has no further reductions."
                            : ""
                    }
                    ${
                        nd._limited || !nd._expanded
                            ? `<br>Double click node or single click and choose:
                                <svg width='1em' height='1em'>
                                    <use href='svg.svg#expand'></use>
                                </svg>.`
                            : ""
                    }`);
    }
    protected _curExample: ExampleMeta = null;
    protected _curLang: Promise<Language> = null;

    get curExample() {
        return this._curExample;
    }

    get curLang() {
        return this._curLang;
    }

    get curQueries() {
        return this.curLang
            .then((l) => l.query || [])
            .then((l) => [...l, ...predefinedQueries]);
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
        const focus = focused ?? this.shower.selectedNode?.data ?? null;
        if (typeof ex?._key !== "string") {
            throw "No example selected!";
        }

        return await getit(`my/example/qry/${ex._key}`, {
            method: "POST",
            headers: new Headers([["Content-Type", "application/json"]]),
            body: JSON.stringify({ qry, focus: focus?._id ?? null }),
        });
    }

    protected async saveQry(
        name: string,
        query: string,
        langInput: Language = null,
    ): Promise<any[]> {
        const lang = langInput ?? (await this.curLang);
        if ((name ?? false) === false || lang === null) {
            throw "No name or example selected";
        }
        const r = await getit(`my/language/${lang._key}/qry`, {
            method: "POST",
            headers: new Headers([["Content-Type", "application/json"]]),
            body: JSON.stringify({ query, name }),
        });

        this.setupQrySelector();
        return r;
    }

    protected get svgCSS() {
        return (
            graphRedexGraphCSS +
            [...this.highlighted]
                .map((h) => {
                    let style = `.highlight-${h.id}{fill: ${h.colour};}`;
                    style += [...this.highlighted]
                        .filter((x) => x.id < h.id)
                        .map(
                            (x) =>
                                `.highlight-${h.id}.highlight-${x.id}{
                                    fill: url("data:image/svg+xml,` +
                                `<svg xmlns='http://www.w3.org/2000/svg'>` +
                                `<linearGradient id='grad'>` +
                                `<stop offset='50%' stop-color='${window.encodeURIComponent(
                                    h.colour,
                                )}'/>` +
                                `<stop offset='50%' stop-color='${window.encodeURIComponent(
                                    x.colour,
                                )}'/></linearGradient></svg>#grad");
                                  } `.replace(/\s\s*/, " "),
                        );

                    return style;
                })
                .join("\n")
        );
    }

    init(plugin: any = null) {
        this.plugin = plugin;
        this.setUpCreateLang();
        this.setupDoReductions();
        this.setupExampleSelector();
        this.setupDoQry();
        this.updateLangs();
        this.setUpShowerBtns();
        this.setUpLegend();
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
        this.setupQrySelector();
        this.shower.setRoot(this.curExample.baseTerm);
        const steps = 300;

        const [data] = await this.doQry(`LET nodes = (
                FOR v,e,p IN 0..${steps}
                    OUTBOUND ${start ? `"${start}"` : "@start"} GRAPH @graph
                    OPTIONS {bfs:true,uniqueVertices: 'global'}
                    RETURN DISTINCT {_id:v._id, _expanded: v._expanded, _stuck: v._stuck, "_limited": (LENGTH(p.edges) == ${steps} && v._expanded && !v._stuck)})
        LET edges = (
            FOR a in nodes
                FOR e IN @@edges
                    FILTER  e._from == a._id OR e._to == a._id
                        RETURN DISTINCT e)
        RETURN {nodes,edges}`);

        if (start) {
            this.shower.push(this.makeNodeProxy(data), startPos);
        } else {
            this.shower.show(this.makeNodeProxy(data));
        }
    }

    async getNodeData(id: string): Promise<GRND> {
        const [data] = await this.doQry(`RETURN DOCUMENT("${id}")`);
        return data;
    }

    private makeNodeProxy(data: { nodes: any; edges: any }): InputData<N, E> {
        return {
            nodes: data.nodes.map((n:{_id:string, _expanded:boolean, _stuck: boolean, _limited: boolean}) => {
                let fullValue = null;
                return new Proxy(n, {
                    get: (
                        target: any,
                        p: string | number | symbol,
                        _receiver: any,
                    ) => {
                        if (p == "_id") return target._id;
                        if (p == "_key") return target._id.split("/").pop();
                        if (
                            ["_stuck", "_limited", "_expanded"].some(
                                (x) => x === p,
                            )
                        )
                            return target[p];
                        if (fullValue === null) {
                            return new Promise((resolve, reject) => {
                                this.getNodeData(target._id)
                                    .then((d: GRND) => {
                                        fullValue = d;
                                        resolve(fullValue[p]);
                                    })
                                    .catch(reject);
                            });
                        } else {
                            return fullValue[p];
                        }
                    },
                    has: (_target: any, p: string | number | symbol) => {
                        if (
                            [
                                "_id",
                                "_key",
                                "term",
                                "_stuck",
                                "_limited",
                                "_expanded",
                            ].some((x) => x === p)
                        )
                            return true;
                        if (fullValue === null) {
                            throw `called "${String(
                                p,
                            )} in ...," on uninitialised node data!`;
                        }
                    },
                    ownKeys(_target: any) {
                        if (fullValue === null) {
                            throw `called ownKeys on uninitialised node data!`;
                        }
                        console.log("keys", Reflect.ownKeys(fullValue));

                        return Reflect.ownKeys(fullValue);
                    },
                    apply: () => {
                        throw "cannot call on node data!";
                    },
                });
            }),
            edges: data.edges,
        };
    }

    /**
     * Render a graph of the argument if it is an array of length 1
     * whose only element has a nodes and edges key
     */
    renderIfGraph(data: any[]) {
        // TODO change to promise
        if (isInputDataArray<N, E>(data)) {
            const [renderData] = data;
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
            if (
                data.length > 0 &&
                data.every((x) => typeof x === "object" && "_id" in x)
            ) {
                if (
                    confirm(
                        "Returned data does not conain any edges. Make a highlight instead?",
                    )
                ) {
                    return this.highlightIfGraph(data);
                }
            } else {
                if (data.length == 0) {
                    throw "result is empty ";
                } else {
                    throw "Rendering nodes is not implemented yet";
                }
            }
        }
        return false;
    }

    /**
     * Render a graph of the argument if it is an array of length 1
     * whose only element has a nodes and edges key
     */
    private randomColor = mkRandomColorGenerator();
    highlightIfGraph(data: any[]): boolean {
        // TODO change to promise
        if (isInputDataArray<N, E>(data)) {
            const [renderData] = data;
            const id = genHighlightId();
            this.highlighted.add({
                edges: new Set(renderData.edges.map((x) => x._id)),
                nodes: new Set(renderData.nodes.map((x) => x._id)),
                name: "highlight " + id,
                id: id,
                colour: this.randomColor().hexFull,
            });
            this.updateHighlightList();
            this.shower.update();
            return true;
        } else {
            if (data.every((x) => typeof x === "object" && "_id" in x)) {
                data = data.map((x) => x._id);
            }

            const d = data as Array<string>;

            // TODO add prefix to exampledata
            const prefix = this.curExample.baseTerm.split("/")[0];
            const id = genHighlightId();
            this.highlighted.add({
                edges: new Set(
                    d.filter((n) => n.startsWith(`${prefix}-reductions/`)),
                ),
                nodes: new Set(d.filter((n) => n.startsWith(`${prefix}/`))),
                name: "highlight " + id,
                id: id,
                colour: this.randomColor().hexFull,
            });

            this.updateHighlightList();
            this.shower.update();
            return true;
        }
    }

    /**
     * Give the data from the query to the plugin (if it exists)
     */
    private executePlugin(data: any[]) {
        if (!this.plugin) {
            throw "Plugin not found";
        }
        return this.plugin(
            {
                render: (d: any) => this.renderIfGraph(d),
                highlight: (d: any[]) => this.highlightIfGraph(d),
                expand: (n: N, d: number) => this.expandBelow(n, d),
            },
            data,
        );
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
    protected async expandNode(node: N) {
        if (!(await node._expanded) && !this.expanding.has(node._id)) {
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
    protected async expandBelow(node: N, depth: number = 50) {
        if (!(await node._expanded)) {
            this.expandNode(node);
        } else {
            const nodes: { _id: string; _key: string }[] = (
                await this.doQry(`FOR v IN 0..${depth}
                    OUTBOUND "${node._id}" GRAPH @graph
                    OPTIONS {bfs:true,uniqueVertices: 'global'}
                    FILTER v._expanded == false
                    RETURN DISTINCT {_key:v._key,_id:v._id}`)
            ).filter((e) => !this.expanding.has(e._id));

            if (nodes.length > 0) {
                // Mark all selected nodes as expanding
                nodes.forEach((n) => this.expanding.add(n._id));

                this.shower.update(); // and update this in the graph

                // wait till they are all expanded
                await Promise.all(
                    nodes.map((n) =>
                        getit(
                            `/continueTerm/${this.curExample._key}/${n._key}`,
                            {
                                method: "POST",
                            },
                        )
                            .then(() => {
                                this.expanding.delete(n._id);
                                this.shower.updateNodeData(n._id, (d) => {
                                    d._expanded = true;
                                });
                            })
                            .catch((e) => {
                                console.error(e);
                                this.expanding.delete(n._id);
                                this.shower.update();
                            }),
                    ),
                );

                this.render(this.curExample, node._id, node._id);
            }
        }
    }

    hlsInited = false;
    protected updateHighlightList() {
        const arr = d3.select("#activeHighlights > ul");
        if (this.highlighted.size > 0 && !this.hlsInited) {
            arr.html("");
        }
        let hls = arr.selectAll("li").data([...this.highlighted]);
        const highlightsEnter = hls.enter().append("li");
        hls.exit().remove();

        highlightsEnter
            .append("span")
            .classed("colourpicker", true)
            .on("click", (d) => {
                d.colour = window.prompt("Colour", d.colour) ?? d.colour;
                this.updateHighlightList();
                this.shower.update();
            });
        highlightsEnter.append("abbr").classed("name", true);
        highlightsEnter
            .append("svg")
            .classed("delHighlight", true)
            .attr("width", "1em")
            .attr("height", "1em")
            .html(`<use fill="#e74c3c" href="svg.svg#remove"></use>`);

        hls = highlightsEnter.merge(hls as any);
        hls.select(".name").text((d) => d.name);
        hls.select(".name").attr(
            "title",
            (d) => `nodes: ${d.nodes.size} | edges:  ${d.edges.size}`,
        );

        hls.select(".colourpicker").style("background", (d) => d.colour);
        hls.select(".delHighlight").on("click", (d) => {
            if (confirm(`Delete ${d.name}?`)) {
                this.highlighted.delete(d);
                this.updateHighlightList();
                this.shower.update();
            }
        });

        hls.on("dblclick", (d) => {
            let newName = window.prompt("Name", d.name);
            if (newName) {
                d.name = newName;
            }
            this.updateHighlightList();
        });
    }

    protected setUpCreateLang() {
        const form = d3.select("#createLanguage");
        const fileInput = form.select<HTMLInputElement>('input[type="file"]');

        const fileInputName = form.select<HTMLInputElement>(
            'input[type="text"]',
        );
        fileInput.on("change", () => {
            if (fileInput.node().files.length === 1) {
                const nameWithouExt = fileInput
                    .node()
                    .files[0].name.replace(/\.[^.]*$/, "");
                fileInputName.property("value", nameWithouExt);
            }
        });

        form.on("submit", () => {
            d3.event.preventDefault();

            const formData = new FormData();
            formData.append("specification", fileInput.node().files[0]);
            formData.append("name", fileInputName.property("value"));

            getit("/my/languages", {
                method: "POST",
                body: formData,
            })
                .then(() => {
                    alert("Language created!");
                    this.updateLangs();
                    form.classed("closed", true);
                })
                .catch(errorAlert);

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
                .catch((data) => {
                    console.error([data]);
                    submitBtn.attr("disabled", null);
                    if (typeof data == "object") {
                        output.textContent = JSON.stringify(data, null, 2);
                        if ("err" in data) {
                            output.textContent = data.err;

                            if ("errors" in data) {
                                const errEl = document.createElement("pre");
                                errEl.textContent = data.errors;
                                output.appendChild(errEl);
                            }
                        }
                    } else {
                        output.textContent = data;
                    }
                });
        });
    }

    protected setupDoQry() {
        const output = document.getElementById("doQryOutput");
        const form = d3.select("#createQry");
        form.selectAll('input[data-type="execute"]').style(
            "display",
            this.plugin ? "" : "none",
        );
        const submitBtn = form.selectAll('input[type="submit"]');
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
                            return this.highlightIfGraph(d);
                        },
                        find: (d: any[]) => {
                            return this.renderIfGraph(d);
                        },
                        execute: (d: any[]) => {
                            return this.executePlugin(d);
                        },
                    }[qryType](data);
                    if (succesfullGraphAction) {
                        output.textContent = "";
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
                    if (typeof error === "object") {
                        output.textContent = error.err ?? JSON.stringify(error);
                    } else {
                        if (typeof error === "string" && error[0] === "<") {
                            output.innerHTML = error;
                        } else {
                            output.textContent = "ERROR:" + error;
                        }
                    }
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
                .text((d: any) => d.name)
                .attr("value", (d: any) => d._key)
                .attr("disabled", null);

            select
                .insert("option", ":first-child")
                .text("Language")
                .attr("value", "")
                .attr("selected", "selected")
                .attr("disabled", "disabled");

            // preselect only lang (if only one)
            if (data.length === 1) {
                options.attr("selected", "selected");
            }
        });
    }

    protected async setupQrySelector() {
        const select = d3.select("#querySelector");
        let options = select.selectAll("option").data(await this.curQueries);
        console.log(await this.curQueries);

        const optionsEnter = options.enter().append("option");
        options.exit().remove();
        options = optionsEnter.merge(options as any);
        options
            .text((d: any) => d.name)
            .attr("value", (d: any) => d.query)
            .attr("disabled", null);

        select
            .insert("option", ":first-child")
            .text("Query")
            .attr("value", "")
            .attr("selected", "selected")
            .attr("disabled", "disabled");

        select.on("change", () => {
            d3.event.preventDefault();
            d3.select("#qry").property("value", select.property("value"));
            d3.select("#qry").dispatch("change");
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
                        d.name + (curKey === d._key ? " (current)" : ""),
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
        this.setupExampleSelector();
        (window as any).toggle(null);
    }

    protected setUpShowerBtns() {
        d3.select("#storeQuery").on("click", async () => {
            d3.event.preventDefault();
            const name = prompt("Name for query");
            if (name) {
                this.saveQry(name, d3.select("#qry").property("value"))
                    .then(() => alert("saved"))
                    .catch(errorAlert);
            }
            return false;
        });

        d3.select("#treeInfoCloseBtn").on("click", () => {
            d3.select("#treeInfoBar")
                .classed("closed", true)
                .classed("max", false);
        });

        d3.select("#treeInfoExpandBtn").on("click", () => {
            d3.select<HTMLElement, any>("#treeInfoBar")
                .node()
                .classList.toggle("max");
        });

        d3.select("#reheat").on("click", () => {
            this.shower.heat();
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

        d3.select("#deleteCurQry").on("click", async () => {
            d3.event.preventDefault();
            const qryName = (d3
                .select("#querySelector")
                .node() as HTMLSelectElement).selectedOptions.item(0).text;
            if (confirm("Do you want to delete\n" + qryName)) {
                getit("my/language/" + (await this.curLang)._key + "/qry", {
                    method: "DELETE",
                    headers: new Headers([
                        ["Content-Type", "application/json"],
                    ]),
                    body: JSON.stringify({ name: qryName }),
                })
                    .then(() => {
                        alert("removed");
                    })
                    .catch(errorAlert);
            }
            return false;
        });
        d3.select("#deleteCurExample").on("click", () => {
            if (confirm("Do you want to delete\n" + this.curExample.name)) {
                getit("my/example/" + this.curExample._key, {
                    method: "DELETE",
                })
                    .then(() => {
                        alert("removed");
                        this.curExample = null;
                        this.reset();
                    })
                    .catch(errorAlert);
            }
        });

        d3.select("#resetFilters").on("click", () => {
            this.reset();
            this.render(this.curExample);
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
                .attr("href", `svg.svg#${this.forceNow ? "tree" : "physics"}`);
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

    protected setUpLegend() {
        const things = [
            { name: "Start", classes: "start" },
            { name: "Finished", classes: "stuck" },
            { name: "Expandable", classes: "expandable" },
        ];
        let legendSVG = d3.select("#graphLegend");
        let nodes = things
            .map(
                ({ classes }, index) =>
                    `<circle class="${classes}" r="10" cx="15" cy="${
                        index * 25 + 15
                    }"></circle>`,
            )
            .join("");
        let names = things
            .map(
                ({ name }, index) =>
                    `<text x="30" y="${index * 25 + 18}">${name}</text>`,
            )
            .join("");
        console.log(nodes, names);
        legendSVG.html(
            `<defs><style>${graphRedexGraphCSS}</style></defs><g class="graph-nodes">${nodes}</g>${names}`,
        );
        legendSVG.attr("viewBox", `0 0 150 ${things.length * 25 + 40}`);
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

/**
 * Checks if the data returned by AQL is renderable by GraphRedex.
 *
 * Helper function to check the type of values returned by AQL.
 *
 * @param data the result array from ArangoDB
 */
function isInputDataArray<N extends GRND, E extends GRED>(
    data: any[],
): data is [InputData<N, E>] {
    if (data.length === 1 && "nodes" in data[0] && "edges" in data[0]) {
        const [{ nodes, edges }] = data;
        if (Array.isArray(nodes) && Array.isArray(edges)) {
            return (
                nodes.every(
                    (x) =>
                        "_id" in x &&
                        "_key" in x &&
                        "term" in x &&
                        "_expanded" in x &&
                        ("_stuck" in x || x._expanded === false),
                ) &&
                edges.every(
                    (x) =>
                        "_id" in x &&
                        "_from" in x &&
                        "_to" in x &&
                        "reduction" in x,
                )
            );
        }
    }
    return false;
}

function errorAlert(e: any) {
    console.error("ERROR", e);
    if (typeof e === "object") {
        if (e.err ?? false) {
            alert("something went wrong\n\n" + e.err);
        } else {
            alert("something went wrong\n\n" + JSON.stringify(e));
        }
    } else {
        alert("something went wrong\n\n" + e);
    }
}
