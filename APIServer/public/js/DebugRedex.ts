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
            nodeMaker: (nodes) => {
                nodes
                    .classed("stuck", (d) => d.data._stuck)
                    .classed("paused", (d) => d.data.action === "pause")
                    .classed("expandable", (d) => !d.data._expanded)
                    .classed(
                        "start",
                        (d) => d.data._id === this.curExample.baseTerm,
                    );

                nodes.on("click", (d) => {
                    d.fx = null;
                    d.fy = null;
                    if (d.data.action === "pause") {
                        this.showDebuggerSteps(d.data);
                    }
                });
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
                    document.getElementsByTagName("section")[0].innerHTML = `
                <pre style="word-wrap: break-word;white-space: pre-wrap; ">${
                    d.data.term
                }</pre>
                <hr>
                <table>
                    <tr><th>Key</th><th>Value</th></tr>
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
                    </table>
                    `;
                });
            },
            nodeUpdate: (nodes) => {
                nodes
                    .classed("expandable", (d) => !d.data._expanded)
                    .classed("expanding", (d) => d.data._expanding || false);
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
        const elem = d3.select(document.getElementsByTagName("section")[1]);
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
}
