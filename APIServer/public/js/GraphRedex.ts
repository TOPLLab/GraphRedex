import { APIDoTermResult, ExampleMeta } from "./_global";
import { getit, fileToText } from "./util";
import Shower from "./Shower";

export default class GraphRedex {
    private curExample: ExampleMeta = null;

    constructor() {
        console.log("Booting graph visualiser");
    }

    init() {
        this.setUpCreateLang();
        this.setupDoReductions();
        this.setupExampleSelector();
        this.setupDoQry();
        this.updateLangs();
        this.setUpTabs();

        console.log(new Shower("svg"));
    }

    render(example: ExampleMeta) {
        console.log(example); //TODO
    }

    renderIfGraph(data: any) {
        // TODO change to promise
        if (data.length === 1) {
            const renderData = data[0];
            const renderKeys = Object.keys(renderData);
            if (renderKeys.includes("nodes") && renderKeys.includes("edges")) {
                console.log(renderData); // TODO
                return true;
            }
        }
        return false;
    }

    protected async doTerm(
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

    private setUpCreateLang() {
        const form = d3.select("#createLanguage").on("submit", () => {
            d3.event.preventDefault();

            const formData = new FormData();
            formData.append(
                "specification",
                form.select('input[type="file"]').node().files[0],
            );

            getit("/my/languages", {
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

    private setupDoReductions() {
        const output = document.getElementById("doReductionOutput");
        const form = d3.select("#doReduction");
        const fileSelector = form.select('input[type="file"]');
        const submitBtn = form.select('input[type="submit"]');
        const termArea = form.select("textarea");
        fileSelector.on("change", async () => {
            termArea.node().value = await fileToText(
                fileSelector.node().files[0],
            );
        });
        form.on("submit", () => {
            submitBtn.attr("disabled", "disabled");

            d3.event.preventDefault();
            output.innerHTML = "Working...";
            const data = termArea.node().value;
            const lang = form.select("#langselector").node().value;
            const name = form.select("#nameselector").node().value;

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

    private setupDoQry() {
        const output = document.getElementById("doQryOutput");
        const form = d3.select("#createQry").on("submit", () => {
            const qry = form.select("textarea").node().value;
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

    private updateLangs() {
        d3.json("/my/languages").then((data) => {
            const select = d3.select("#langselector");
            const options = select.selectAll("option").data(data);
            options.enter().append("option");
            options.exit().remove();
            select
                .selectAll("option")
                .text((d) => d.name + " - " + d._key)
                .attr("value", (d) => d._key)
                .attr("disabled", null);

            select
                .insert("option", ":first-child")
                .text("Language")
                .attr("value", "")
                .attr("selected", "selected")
                .attr("disabled", "disabled");
        });
    }

    private setupExampleSelector() {
        d3.json("/my/examples").then((data) => {
            const select = d3.select("#exampleSelector").on("change", () => {
                const si = select.property("value"),
                    s = options.filter((d) => d._key === si),
                    data = s.datum();
                this.render(data);
            });
            console.log(data.map((d) => d.name + " - " + d._key));
            const options = select.selectAll("option").data(data);
            options.enter().append("option");
            options.exit().remove();
            select
                .selectAll("option")
                .attr("disabled", null)
                .text((d) => d.name + " - " + d._key)
                .attr("value", (d) => d._key);

            select
                .insert("option", ":first-child")
                .text("Example")
                .attr("value", "")
                .attr("selected", "selected")
                .attr("disabled", "disabled");
        });
    }

    setUpTabs(): any {
        window.toggle = (elID: string) => {
            // tslint:disable-line
            document.querySelectorAll("main>form").forEach((e: Element) => {
                if (e.id !== elID) {
                    e.classList.add("closed");
                }
            });
            document.getElementById(elID).classList.toggle("closed");
        };
    }
}
