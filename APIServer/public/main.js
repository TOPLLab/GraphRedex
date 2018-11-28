

/**
 * Do a check
 * @param {*} getStatus
 * @param {*} onChange
 * @param {*} interval
 */
function setCheckInterval(getStatus, onChange, interval) {
    let oldStatus = getStatus();
    window.setInterval(() => {
        const newStatus = getStatus();
        if (oldStatus != newStatus) {
            console.log("update to ", newStatus);
            onChange(newStatus);
        }
        oldStatus = newStatus;
    }, interval);
    onChange(oldStatus);
}


(() => {
    /**
 * Update the language selectors
 */
    function updateLangs() {
        d3.json("/my/languages").then(data => {
            const select = d3.select("#langselector");
            const options = select.selectAll("option").data(data);
            options.enter().append("option");
            options.exit().remove();
            select.selectAll("option")
                .text(d => d.name + " - " + d._key)
                .attr("value", d => d._key)
                .attr("disabled", null);

            select.insert("option", ":first-child")
                .text("Language")
                .attr("value", "")
                .attr("selected", "selected")
                .attr("disabled", "disabled");
        });
    }

    /**
 * Setup the create new language menu
 */
    function setupCreateLang() {
        const form = d3.select("#createLanguage").on("submit", () => {
            d3.event.preventDefault();

            const formData = new FormData();
            formData.append("specification", form.select("input[type=\"file\"]").node().files[0]);
            formData.append("name", form.select("input[type=\"text\"]").node().value);


            window.fetch("/my/languages", {
                method: "POST",
                body: formData,
            })
                .then(x => x.json())
                .then(data => {
                    if (data.ok) {
                        alert("Language created!");
                        updateLangs();
                    } else {
                        alert("something went wrong");
                    }
                })
                .catch(e => {
                    console.log("ERROR", e); alert("something went wron");
                });


            return false;
        });
    }

    /**
 * Get example data and render it
 * @param {Number} id
 */
    function renderExample(id) {
        d3.json("/my/example/show/" + id).then(visualise);
    }

    /**
 * Listen to events of the example dropdown
 */
    function setupExampleSelector() {
        d3.json("/my/examples").then(data => {
            const select = d3.select("#exampleSelector")
                .on("change", () => {
                    renderExample(select.property("value"));
                });
            console.log(data.map(d => d.name + " - " + d._key));
            const options = select.selectAll("option").data(data);
            options.enter().append("option");
            options.exit().remove();
            select.selectAll("option")
                .text(d => d.name + " - " + d._key)
                .attr("value", d => d._key);

            select.insert("option", ":first-child")
                .text("Example")
                .attr("value", "")
                .attr("selected", "selected")
                .attr("disabled", "disabled");
        });
    }

    /**
 * Setup
 */
    function setupDoReductions() {
        const output = document.getElementById("doReductionOutput");
        const form = d3.select("#doReduction").on("submit", () => {
            const submitBtn = form.select("input[type=\"submit\"]")
                .attr("disabled", "disabled");

            d3.event.preventDefault();
            output.innerHTML = "Working...";
            const data = form.select("textarea").node().value;
            const lang = form.select("#langselector").node().value;
            const name = form.select("#nameselector").node().value;


            if (name.length <= 0) {
                output.textContent = "! Provide a name";
                submitBtn.attr("disabled", null);
                return;
            }

            if (lang.length <= 0) {
                output.textContent = "! Provide a language";
                submitBtn.attr("disabled", null);
                return;
            }


            window.fetch(`/doTerm/${lang}/${name}`, {method: "POST", body: data})
                .then(result => {
                    submitBtn.attr("disabled", null);
                    if (result.ok) {
                        result.text()
                            .catch(e => {
                                document.getElementById("doReductionOutput").textContent = "Something went wrong" + e;
                            })
                            .then(txt => {
                                const data = JSON.parse(txt);
                                output.textContent = "succes";
                                d3.json("/my/example/show/" + data.example._key).then(visualise);
                                document.getElementById("doReduction").classList.toggle("closed");
                            })
                            .catch(e => {
                                console.log("Error", e); output.textContent = e;
                            });
                    } else {
                        output.textContent = "Something went wrong";
                        result.text().then(d => {
                            try {
                                const data = JSON.parse(d);
                                console.log("BO", data);
                                if ("e" in data) {
                                    output.textContent = data.e;

                                    if ("errors" in data) {
                                        const errEl = document.createElement("pre");
                                        errEl.textContent = data.errors;
                                        output.appendChild(errEl);
                                    }
                                } else {
                                    const errEl = document.createElement("pre");
                                    errEl.textContent = JSON.stringify(data, null, 2);
                                    output.innerHTML = "";
                                    output.appendChild(errEl);
                                }
                            } catch (e) {
                                output.textContent = d;
                            }
                        });
                    }
                })
                .catch(e => {
                    submitBtn.attr("disabled", null);
                    console.log("Error", e); document.getElementById("doReductionOutput").innerText = e;
                });
        });
    }

    let svgRoot, defs, graph;

    const existing = new Map();
    /**
     * Get a random color for a name (or arrow)
     * @param {string} t name
     * @param {boolean} arrow make an arrow
     * @return {string} color or reference to arrow
     */
    function getRandCol(t, arrow = false) {
        if (!existing.has(t)) {
            const letters = "0123456789ABCDEF";
            let resCol = "";
            for (let i = 0; i < 6; i++) {
                resCol += letters[Math.floor(Math.random() * 16)];
            }
            existing.set(t, resCol);

            const markersize = 3;
            defs.append("svg:marker")
                .attr("id", "marker-" + resCol)
                .attr("refX", 10)
                .attr("refY", markersize / 2)
                .attr("markerWidth", 30)
                .attr("markerHeight", 30)
                .attr("orient", "auto")
                .append("path")
                .attr("d", `M 0 0 ${markersize} ${markersize / 2} 0 ${markersize} ${markersize / 4} ${markersize / 2}`)
                .style("fill", "#" + resCol);
        }
        const color = existing.get(t);
        return arrow ? "marker-" + color : "#" + color;
    }
    /**
     * Show an example on the svg
     * @param {Example} data
     */
    function visualise(data) {
        const startNode = ("meta" in data) ? data.meta.baseTerms[0] : null;

        const links = data.edges.map(d => ({source: d._from, target: d._to, reduction: d.reduction}));
        const nodes = data.nodes.map(d => ({id: d._id, term: d.term, data: d}));
        const simulation = forceSimulation(nodes, links);
        simulation.on("tick", ticked);
        simulation.velocityDecay(0.1);


        let link = graph.arrow().data(links);
        link.exit().remove();
        link.enter().append("path");

        link = graph.arrow();
        link.attr("stroke", d => getRandCol(d.reduction))
            .attr("marker-end", d => "url(#" + getRandCol(d.reduction, true) + ")");


        let linkText = graph.text().data(links);
        linkText.exit().remove();
        linkText.enter().append("text");

        linkText = graph.text();
        linkText.attr("text-anchor", "middle")
            .attr("fill", d => getRandCol(d.reduction))
            .text(d => d.reduction);

        let node = graph.node().data(nodes);
        node.exit().remove();
        node.enter().append("circle");

        node = graph.node();
        node.attr("class", "term-node")
            .attr("fill", d => ((d.data._id === startNode) ? "#0f0" : "#00f"))
            .attr("r", 10)
            .on("click", d => {
                d.fx = null; d.fy = null;
            })
            .on("mouseover", d => {
                document.getElementsByTagName("section")[0].innerHTML = `
        <pre style="word-wrap: break-word;white-space: pre-wrap; ">${d.term}</pre>
        <hr>
        <table>
            <tr><th>Key</th><th>Value</th></tr>
            ${Object.entries(d.data).filter(x => !x[0].startsWith("_") && x[0] != "term").map(x => `<tr><td>${x[0]}</td><td>${x[1]}</td></tr>`).join("")}
            </table>
            `;
            })
            .call(drag(simulation));


        /* hide text when too far */
        setCheckInterval(() => (graph.node().node().getBoundingClientRect().height > 40),
            status => {
                graph.text().attr("opacity", status ? "1" : "0");
            },
            1000);

        node.append("title").text(d => d.id);


        /**
     * Single tick of the simulation
     */
        function ticked() {
            link.attr("d", d => {
                const dx = d.target.x - d.source.x;
                const dy = d.target.y - d.source.y;
                return `M ${d.source.x} ${d.source.y} q ${dx / 2} ${dy / 2} ${dx} ${dy}`;
            });

            linkText
                .attr("x", d => (d.source.x + d.target.x) / 2)
                .attr("y", d => (d.source.y + d.target.y) / 2)
                .attr("transform", d => `rotate(${
                    180 * Math.atan((d.source.y - d.target.y) / (d.source.x - d.target.x)) / Math.PI},
    ${(d.source.x + d.target.x) / 2},
    ${(d.source.y + d.target.y) / 2}
    )`);

            node
                .attr("cx", d => d.x)
                .attr("cy", d => d.y);
        }
    }

    /**
     * Create a the force simulation
     * @param {Node[]} nodes
     * @param {Link[]} links
     * @return {any} Force sim
     */
    function forceSimulation(nodes, links) {
        return d3.forceSimulation(nodes)
            .force("link", d3.forceLink(links).distance(30).strength(1.5).id(d => d.id))
            .force("charge", d3.forceManyBody().strength(-15))
            .force("collide", d3.forceCollide(16).strength(0.5))
            .force("center", d3.forceCenter());
    }


    /**
     * Simulate drag
     * @param {any} simulation
     * @return {any} drag
     */
    function drag(simulation) {
        return d3.drag()
            .on("start", d => {
                if (!d3.event.active) simulation.alphaTarget(0.3).restart();
                d.fx = d.x;
                d.fy = d.y;
            })
            .on("drag", d => {
                d.fx = d3.event.x;
                d.fy = d3.event.y;
            })
            .on("end", d => {
                if (!d3.event.active) simulation.alphaTarget(0);
                // d.fx = null;
                // d.fy = null;
            });
    }


    /**
     * Initialise
     */
    function init() {
        console.log("Welcome");
        setupCreateLang();
        setupDoReductions();
        setupExampleSelector();
        updateLangs();
        const width = 1000;
        const height = 1000;
        // Create SVG element
        svgRoot = d3.select("svg")
            .attr("width", width)
            .attr("height", height)
            .attr("viewBox", [-width / 2, -height / 2, width, height])
            .call(d3.zoom().on("zoom", () => {
                svg.attr("transform", d3.event.transform);
            }));
        defs = svgRoot.append("svg:defs");
        const svg = svgRoot.append("g");

        const arrowSVG = svg.append("g").attr("class", "graph-arrows");
        const textSVG = svg.append("g").attr("class", "graph-texts");
        const nodeSVG = svg.append("g").attr("class", "graph-nodes");

        graph = {
            node: () => nodeSVG.selectAll("circle"),
            arrow: () => arrowSVG.selectAll("path"),
            text: () => textSVG.selectAll("text"),
        };
    };


    document.addEventListener("DOMContentLoaded", init);
})();
