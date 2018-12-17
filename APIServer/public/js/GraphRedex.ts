import { APIDoTermResult, ExampleMeta, TermMeta } from "./_global";
import { getit, fileToText } from "./util";
import Shower from "./shower/Shower";

export default class GraphRedex {
    protected curExample: ExampleMeta = null;
    protected shower: Shower;

    constructor(showerConfig: ShowerConfig = null) {
        console.log("Booting graph visualiser");
        this.shower = new Shower(
            "svg",
            showerConfig || {
                nodeMaker: (nodes: any) => {
                    nodes.classed(
                        "start",
                        (d) => d.data._id === this.curExample.baseTerm,
                    );

                    const firstNode = nodes.filter(
                        (d) => d.data._id === this.curExample.baseTerm,
                    );
                    if (firstNode.size() === 1) {
                        firstNode.datum().fx = 0;
                        firstNode.datum().fy = 0;
                    }

                    nodes.on("click", (d) => {
                        console.log("clicked-", d);
                        d.fx = null;
                        d.fy = null;
                    });
                    nodes.on("dblclick", (d) => {
                        d3.event.preventDefault();
                        d3.event.stopPropagation();
                        console.log("dblclick", d);
                        if (!d.data._expanded) {
                            d.data._expanding = true;
                            this.shower.update();
                            this.expandNode(d.data).then(() => {
                                d.data._expanding = false;
                                d.data._expanded = true;
                                this.shower.update();
                            });
                        }
                    });
                    nodes.on("mouseover", (d) => {
                        document.getElementsByTagName(
                            "section",
                        )[0].innerHTML = `
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
                nodeUpdate: (nodes: any) => {
                    nodes
                        .classed("expandable", (d) => !d.data._expanded)
                        .classed(
                            "expanding",
                            (d) => d.data._expanding || false,
                        );
                },
            },
        );
    }

    init() {
        this.setUpCreateLang();
        this.setupDoReductions();
        this.setupExampleSelector();
        this.setupDoQry();
        this.updateLangs();
        this.setUpReheatBtn();
    }

    async render(
        example: ExampleMeta,
        start: string = null,
        startPos: string = null,
    ) {
        this.curExample = example;
        this.shower.setRoot(this.curExample.baseTerm);
        const steps = 300;

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

    protected async expandNode(node: TermMeta) {
        console.log("expanding", node._key);
        await getit(`/continueTerm/${this.curExample._key}/${node._key}`, {
            method: "POST",
        });
        await this.render(this.curExample, node._id, node._id);
    }

    renderIfGraph(data: any) {
        // TODO change to promise
        if (data.length === 1) {
            const renderData = data[0];
            const renderKeys = Object.keys(renderData);
            if (renderKeys.includes("nodes") && renderKeys.includes("edges")) {
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
        const fileSelector = form.select('input[type="file"]');
        const submitBtn = form.select('input[type="submit"]');
        const termArea = form.select("textarea");
        fileSelector.on("change", async () => {
            termArea.property(
                "value",
                await fileToText(fileSelector.property("files")[0]),
            );
        });
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
                    output.textContent = "succes";
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
            const qry = form.select("textarea").property("value");
            console.log(qry, this.curExample);

            output.textContent = "Wait for it...";
            getit("my/example/qry/" + this.curExample._key, {
                method: "POST",
                body: qry,
            })
                .then((data) => {
                    output.textContent =
                        "Done: See developper console for output\n";
                    console.log("\n\nQry:\n" + qry);
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
            options.exit().remove();
            options = optionsEnter.merge(options as any);
            options
                .attr("disabled", null)
                .text((d: any) => d.name + " - " + d._key)
                .attr("value", (d: any) => d._key);

            select
                .insert("option", ":first-child")
                .text("Example")
                .attr("value", "")
                .attr("selected", "selected")
                .attr("disabled", "disabled");
        });
    }

    protected setUpReheatBtn() {
        d3.select("#reheat").on("click", () => {
            this.shower.heatFor(5000);
        });
    }
}
