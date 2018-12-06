export default class Shower {
    private svgRoot: any;
    private width: number = 1000;
    private height: number = 1000;
    private zoomHandler: d3.ZoomBehavior<any, any>;
    private defs: any;
    private scene: any;
    private parts: { nodes: any; arrows: any; texts: any };

    constructor(svg: any) {
        this.svgRoot = d3.select(svg);
        this.svgRoot
            .attr("width", this.width)
            .attr("height", this.height)
            .attr("viewBox", [
                -this.width / 2,
                -this.height / 2,
                this.width,
                this.height,
            ])
            .call(
                (this.zoomHandler = d3.zoom().on("zoom", () => {
                    svg.attr("transform", d3.event.transform);
                })),
            );
        this.defs = this.svgRoot.append("svg:defs");
        this.scene = this.svgRoot.append("g");

        this.parts = {
            nodes: this.scene
                .append("g")
                .classed("graph-nodes", true)
                .selectAll("circle")
                .data([]),
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
        };
    }

    public resetZoom() {
        this.svgRoot.call(this.zoomHandler.transform, d3.zoomIdentity);
    }
}
