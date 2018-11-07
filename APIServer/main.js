var express = require("express");
var app = express();
var aql = require("arangojs").aql;
const { spawn } = require("child_process");
var stream = require("stream");

var db = require("arangojs")();
db.useBasicAuth("graphredex", "graphredex");
db.useDatabase("graphredex-test");

let lastStart = null;

app.get("/demo", function(req, res) {
    const startnode = lastStart || "terms-test-1/252278";
    const graph = "terms-test-1";
    const reductionCollection = db.collection(graph + "-reductions");
    db.query(
        aql`
            LET docId = ${startnode}
            LET nodes = (FOR v IN 1..${2000} OUTBOUND docId GRAPH ${graph} OPTIONS {bfs:true,uniqueVertices: 'global'} RETURN v)
            LET nodesID = (FOR v IN nodes RETURN v._id)
            LET edges = (FOR a in nodesID FOR e in ${reductionCollection} FILTER e._from == a AND POSITION(nodesID,e._to) == true RETURN DISTINCT e)
            RETURN {nodes,edges,nodesID}`,
    )
        .then((cursor) => cursor.all())
        .then(
            (keys) => res.jsonp(keys[0]),
            (err) => res.status(500).jsonp({ error: err }),
        );
});

app.post("/doTerm/:lang", function(req, res) {
    var data = "";
    req.setEncoding("utf8");
    req.on("data", function(chunk) {
        data += chunk;
    });

    req.on("end", function() {
        req.body = data;

        var child = spawn(
            "racket",
            ["RedexServer/" + req.params.lang + ".rkt"],
            {
                cwd:
                    "/home/beardhatcode/Documents/doctoraat/RelatedWork/GraphRedex/code",
                env: { LC_ALL: "C" },
                stdio: ["pipe", "pipe", "pipe"],
            },
        );

        console.log(child);

        var stdinStream = new stream.Readable();
        stdinStream.push(req.body); // Add data to the internal queue for users of the stream to consume
        stdinStream.push(null); // Signals the end of the stream (EOF)
        stdinStream.pipe(child.stdin);

        let output = "";
        child.stderr.on("data", (data) => {
            console.log(`stdout: ${data}`);
        });

        child.stdout.on("data", (data) => {
            output += data.toString();
        });

        child.on("close", (code) => {
            console.log(`child process exited with code ${code}`);
            res.jsonp({
                lang: req.params.lang,
                body: req.body,
                out: output,
                code: code,
            });
            lastStart = output;
        });
    });
});

module.exports = app;
