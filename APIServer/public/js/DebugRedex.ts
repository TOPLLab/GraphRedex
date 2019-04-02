import { ExampleMeta, TermMeta } from "./_global";
import { getit } from "./util";
import GraphRedex from "./GraphRedex";
import * as d3 from "d3";

interface GRND extends NodeData {
    _id: string;
    _key: string;
    term: string;
    _stuck: boolean;
    _limited?: boolean;
    _expanded: boolean;
    action: string;
    _expanding?: boolean;
}
interface GRED extends EdgeData {
    _id: string;
    _from: string;
    _to: string;
    reduction: string;
    _real: boolean;
}

export default class DebugRedex extends GraphRedex<GRND, GRED> {
    constructor() {
        super({
            nodeSelected: (node) => {
                if (node.data.action === "pause") {
                    this.showDebuggerSteps(node.data);
                }
                return true;
            },
            nodeOptions: (node) => {
                if (node.data.action === "pause") {
                    const debugStep = (name, edgename, icon, size = 1) => ({
                        name: name,
                        icon: icon,
                        size: size,
                        action: async () => {
                            const steps = await this.getNonRealSteps(node.data);
                            const step = steps.filter(
                                (x) => x.name === edgename,
                            )[0]; // TODO add check
                            this.render(
                                this.curExample,
                                step._to,
                                node.data._id,
                            );
                            return false;
                        },
                    });
                    return [
                        debugStep("resume", "Resume-Execution", "resume", 3),
                        debugStep("pause", "Pause-Execution", "pause", 1),
                        debugStep(
                            "step-into",
                            "Step-Msg-Receiver",
                            "step-into",
                            2,
                        ),
                        debugStep(
                            "step-into",
                            "Step-Future-Resolution",
                            "step-over",
                            2,
                        ),
                        {
                            name: "unpin",
                            action: () => {
                                const n = node as any;
                                n.fx = null;
                                n.fy = null;
                                return true;
                            },
                            icon: "unpin",
                            size: 1,
                        },
                        {
                            name: "info",
                            action: () => {
                                alert(node.data.term);
                                return true;
                            },
                            icon: "info",
                            size: 1,
                        },
                    ];
                } else {
                    return [
                        {
                            name: "expand",
                            icon: "expand",
                            size: 2,
                            action: () => {
                                this.expandBelow(node.data);
                                return false;
                            },
                        },
                        {
                            name: "unpin",
                            action: () => {
                                const n = node as any;
                                n.fx = null;
                                n.fy = null;
                                return false;
                            },
                            icon: "unpin",
                            size: 1,
                        },
                        {
                            name: "info",
                            action: () => {
                                alert(node.data.term);
                                return false;
                            },
                            icon: "info",
                            size: 1,
                        },
                    ];
                }
            },
            nodeMaker: (nodes) => {
                nodes
                    .classed("stuck", (d) => d.data._stuck)
                    .classed("paused", (d) => d.data.action === "pause")
                    .classed("expandable", (d) => !d.data._expanded)
                    .classed(
                        "start",
                        (d) => d.data._id === this.curExample.baseTerm,
                    );

                nodes.on("dblclick", (d) => {
                    d3.event.preventDefault();
                    d3.event.stopPropagation();

                    if (d3.event.shiftKey) {
                        // get all unexpanded node within 50 steps and expand them
                        this.expandBelow(d.data);
                    } else {
                        this.expandNode(d.data);
                    }
                });
                nodes.on("mouseover", (d) => {
                    document.getElementById("statusSection").innerHTML = `
                <pre style="word-wrap: break-word;white-space: pre-wrap; ">${
                    d.data.term
                }</pre>
                <hr>
                <table class="pure-table pure-table-horizontal stretch"><thead>
                    <tr><th>Key</th><th>Value</th></tr></thead><tbody>
                    ${Object.keys(d.data)
                        .filter(
                            (x) =>
                                ["_stuck", "_expanded"].includes(x) ||
                                !(x.startsWith("_") || x === "term"),
                        )
                        .map(
                            (x) =>
                                `<tr><td>${x}</td><td>${d.data[x]}</td></tr>`,
                        )
                        .join("")}
                    </tbody></table>
                    `;
                });
            },
            nodeUpdate: (nodes) => {
                nodes
                    .classed("expandable", (d) => !d.data._expanded)
                    .classed("expanding", (d) => this.expanding.has(d.id));
            },
        });
    }

    async render(
        example: ExampleMeta,
        start: string = null,
        startPos: string = null,
    ) {
        this.curExample = example;
        this.shower.setRoot(this.curExample.baseTerm);
        const steps = 3000;

        const [data] = await getit("my/example/qry/" + example._key, {
            method: "POST",
            body: `LET nodes = (
                FOR v,e,p IN 0..${steps}
                    OUTBOUND ${start ? `"${start}"` : "@start"} GRAPH @graph
                    OPTIONS {bfs:true,uniqueVertices: 'global'}
                    FILTER p.edges[*]._real NONE == false
                    RETURN DISTINCT v)
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

    private async showDebuggerSteps(node: TermMeta) {
        const elem = d3.select("#debuggerSection");
        elem.html(
            `<h1>Debug</h1><small>${node._id} (node ${
                node._expanded ? "has been" : " <strong>is being<strong>"
            } expanded)</small>`,
        );

        if (!node._expanded) {
            await this.expandNode(node);
        }

        elem.html(
            `<h1>Debug</h1><small>${node._id} (node has been expanded)</small>`,
        );

        this.getNonRealSteps(node).then((possibleSteps) => {
            const list = elem.append("ul");
            for (const { name, _to: target } of possibleSteps) {
                list.append("li")
                    .append("button")
                    .text(name)
                    .on("click", () => {
                        this.render(this.curExample, target, node._id);
                        list.remove();
                        elem.append("div").text(`${name} performed`);
                    });
            }
        });
    }

    protected setUpCreateLang() {
        const form = d3.select("#createLanguage").on("submit", () => {
            d3.event.preventDefault();

            const formData = new FormData();
            const a: any = form.select('input[type="file"]').node();
            formData.append("specification", a.files[0]);

            getit("/my/languages/debugger", {
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

    protected get svgCSS() {
        return `.graph-arrows {
            stroke-width: 2;
            fill: transparent;
        }

        .graph-nodes {
            fill: rgb(86, 198, 212);
            stroke: #ffffff;
            stroke-width: 2;
        }

        .graph-nodes .expandable {
            stroke: grey;
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


        .graph-nodes .paused {
            fill: pink;
        }

        .graph-texts {
            font-size: 3px;
            font-family: "Noto Sans","Courier New",monospace;
        }`;
    }
}
