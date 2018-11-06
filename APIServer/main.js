var express = require("express");
var app = express();
var db = require("arangojs")();
var aql = require("arangojs").aql;

db.useBasicAuth("graphredex", "graphredex");
db.useDatabase("graphredex-test");

app.get("/demo", function(req, res) {
    const startnode = "terms-test-1/170312";
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

module.exports = app;
