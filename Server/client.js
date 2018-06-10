/*
  ________                    .__      __________           .___             
 /  _____/___________  ______ |  |__   \______   \ ____   __| _/____ ___  ___
/   \  __\_  __ \__  \ \____ \|  |  \   |       _// __ \ / __ |/ __ \\  \/  /
\    \_\  \  | \// __ \|  |_> >   Y  \  |    |   \  ___// /_/ \  ___/ >    < 
 \______  /__|  (____  /   __/|___|  /  |____|_  /\___  >____ |\___  >__/\_ \
        \/           \/|__|        \/          \/     \/     \/    \/      \/
 Christophe.Scholliers@UGent and Thomas.Dupriez@ens-paris-saclay.fr
*/

function connectToNeo4jDatabase() {
    var authToken = neo4j.v1.auth.basic("neo4j", "neo4j-js-password");
    console.log(authToken);
    var driver = neo4j.v1.driver("bolt://localhost", authToken, {
        encrypted: false,
    });
    session = driver.session();
}

function runCypherStatement(statement) {
    session.run(statement).subscribe({
        onNext: function(record) {
            // On receipt of RECORD
            // var tr = document.createElement("tr");
            // record.forEach( function( value ) {
            //     var td = document.createElement("td");
            //     td.appendChild(document.createTextNode(value));
            //     tr.appendChild(td);
            // });
            // table.appendChild(tr);
        },
        onCompleted: function(metadata) {},
    });
}

function valueToStringForCypherStatement(value) {
    if (typeof value === "string") {
        return '"' + value + '"';
    } else {
        return "" + value;
    }
}

window.onload = function() {
    connectToNeo4jDatabase();
    var sock = new WebSocket("ws://localhost:8081/");
    var messagecounter = 0;
    sock.onopen = function() {
        console.log("open", arguments);
    };
    sock.onmessage = function(e) {
        var obj = JSON.parse(e.data);
        // console.log("obj= "+obj)
        // var from = obj.from;
        // console.log("from object:");
        // console.log(from);
        // obj = obj.next;
        // console.log("next object:");
        // console.log(obj);
        // for (var i in obj) {
        //     console.log((obj[i]).term);
        //     if (!isTermAlreadyInTheDatabase((obj[i]).term)) {
        //         addTermObjectToDatabase(obj[i]);
        //     }
        // }

        getOrCreateNodeForTermObject(obj.from, (fromNodeID) => {
            var reductionTermObjects = obj.next;
            for (var i in reductionTermObjects) {
                var reductionTermObject = reductionTermObjects[i];
                getOrCreateNodeForTermObject(
                    reductionTermObject,
                    (reductionNodeID) => {
                        setReducesToRelationFromSourceNodeToTargetNodeIfNotAlreadyThere(
                            fromNodeID,
                            reductionNodeID,
                        );
                    },
                );
            }
        });

        // MATCH (e) WHERE ID(e)=99 MATCH (f) WHERE ID(f)=100 CREATE (e)-[:REDUCESTO]->(f)

        //console.log('message', e.data);
    };
    sock.onclose = function() {
        console.log("close", arguments);
    };
    window.send = function() {
        var term = editor.getValue();
        sock.send(term);
    };
    window.emptyDatabase = function() {
        runCypherStatement("MATCH (n) OPTIONAL MATCH (n)-[r]-() DELETE n,r");
        console.log("Neo4j Database emptied");
    };
    window.setReducesToRelationFromSourceNodeToTargetNodeIfNotAlreadyThere = function(
        sourceNodeID,
        targetNodeID,
    ) {
        doesSourceNodeReducesToTargetNode(
            sourceNodeID,
            targetNodeID,
            (relationExists) => {
                if (!relationExists) {
                    setReducesToRelationFromSourceNodeToTargetNode(
                        sourceNodeID,
                        targetNodeID,
                    );
                }
            },
        );
    };
    window.doesSourceNodeReducesToTargetNode = function(
        sourceNodeID,
        targetNodeID,
        callback,
    ) {
        // console.log("doesSourceNodeReducesToTargetNode");
        // console.log("sourceNodeID="+sourceNodeID+" targetNodeID="+targetNodeID);
        var cypherStatement =
            "MATCH (e)-[:REDUCESTO]->(f) WHERE ID(e)=" +
            sourceNodeID +
            " AND ID(f)=" +
            targetNodeID +
            " RETURN ID(e)";
        // console.log("cypherStatement= "+cypherStatement);
        session.run(cypherStatement).then(
            (result) => {
                // console.log("result=");
                // console.log(result);
                // console.log("answer= "+(result.records.length >= 1));
                callback(result.records.length >= 1);
            },
            (error) => {
                console.log(
                    "ERROR: cypher statement error in function doesSourceNodeReducesToTargetNode",
                );
                console.log("code: " + error.code + " | " + error.message);
            },
        );
    };
    window.setReducesToRelationFromSourceNodeToTargetNode = function(
        sourceNodeID,
        targetNodeID,
    ) {
        var cypherStatement =
            "MATCH (e) WHERE ID(e)=" +
            sourceNodeID +
            " MATCH (f) WHERE ID(f)=" +
            targetNodeID +
            " CREATE (e)-[:REDUCESTO]->(f)";
        session.run(cypherStatement).then(
            (result) => {
                console.log(
                    "Set ReducesTo relation from node of ID " +
                        sourceNodeID +
                        " to node of ID " +
                        targetNodeID,
                );
            },
            (error) => {
                console.log(
                    "ERROR: cypher statement error in function setReducesToRelationFromSourceNodeToTargetNode",
                );
                console.log("code: " + error.code + " | " + error.message);
            },
        );
    };
    window.getOrCreateNodeForTermObject = function(termObject, callback) {
        // Given a termObject, checks whether a node with the same term exists in the database.
        // If there is one, calls the callback with its ID as argument.
        // If there is none, it creates one, and calls the callback with its ID as argument.
        var term = termObject.term;
        isTermAlreadyInTheDatabase(term, (falseOrID) => {
            if (falseOrID == false) {
                // No node exist already for this termObject. Creating one.
                addTermObjectToDatabase(termObject, callback);
            } else {
                // A node exists for this termObject.
                callback(falseOrID);
            }
        });
    };
    window.isTermAlreadyInTheDatabase = function(term, callback) {
        // Returns false if a node with the same term as the argument is in the database
        // Otherwise, returns the id of a node that has the same term as the argument.
        var statement = 'MATCH (e) WHERE e.term = "' + term + '" RETURN ID(e)';
        // console.log("isTermAlreadyInTheDatabase");
        session.run(statement).then(
            (result) => {
                var records = result.records;
                var summary = result.summary;
                // console.log("records= ");
                // console.log(records);
                if (records.length >= 1) {
                    var id = records[0]._fields[0].low;
                    // console.log("Term already in the database. ID= " + id);
                    // for (var k in records[0]) {
                    //     console.log("k=");
                    //     console.log(k);
                    //     console.log("v=");
                    //     console.log(records[0][k]);
                    // }
                    callback(id);
                } else {
                    // console.log("Term NOT already in the database");
                    callback(false);
                }
            },
            (error) => {
                console.log(
                    "ERROR: cypher statement error in function isTermAlreadyInTheDatabase",
                );
                console.log("code: " + error.code + " | " + error.message);
            },
        );
    };
    window.addTermObjectToDatabase = function(termObject, callback) {
        // Given a termObject, creates a node for it in the database, then call callback with no argument.
        // console.log("termObject= ");
        // console.log(termObject);
        var term = termObject[term];
        var cypherStatement = "CREATE (e:Term {";
        cypherStatementEnd = "}) RETURN ID(e)";
        var addComma = false;
        for (k in termObject) {
            if (addComma) {
                cypherStatement = cypherStatement + ", ";
            } else {
                addComma = true;
            }
            cypherStatement =
                cypherStatement +
                k +
                ": " +
                valueToStringForCypherStatement(termObject[k]);
        }
        cypherStatement = cypherStatement + cypherStatementEnd;
        // console.log("Cypher statement: "+cypherStatement);
        session.run(cypherStatement).then(
            (result) => {
                var records = result.records;
                var nodeID = records[0]._fields[0].low;
                console.log(
                    "Added term node of ID " + nodeID + " to database.",
                );
                callback(nodeID);
            },
            (error) => {
                console.log(
                    "ERROR: cypher statement error in function addTermObjectToDatabase",
                );
                console.log("code: " + error.code + " | " + error.message);
            },
        );
    };
};
