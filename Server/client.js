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
        var from = obj.from;
        console.log("from object:");
        console.log(from);
        obj = obj.next;
        console.log("next object:");
        console.log(obj);
        for (var i in obj) {
            console.log(obj[i].term);
            if (!isTermAlreadyInTheDatabase(obj[i].term)) {
                addTermNodeToDatabase(obj[i]);
            }
        }

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
        runCypherStatement("MATCH (n) DELETE n");
        console.log("Neo4j Database emptied");
    };
    window.isTermAlreadyInTheDatabase = function(term) {
        // Returns false if a node with the same term as the argument is in the database
        // Otherwise, returns the id of a node that has the same term as the argument.
        var statement = 'MATCH (e) WHERE e.term = "' + term + '" RETURN ID(e)';
        console.log("isTermAlreadyInTheDatabase");
        session.run(statement).then(
            (result) => {
                var records = result.records;
                var summary = result.summary;
                console.log("records= ");
                console.log(records);
                if (records.length >= 1) {
                    console.log("Yes. ID= ");
                    console.log(records[0]);
                    for (var k in records[0]) {
                        console.log("k=");
                        console.log(k);
                        console.log("v=");
                        console.log(records[0][k]);
                    }
                } else {
                    console.log("No");
                }
            },
            (error) => {
                console.log("cypher statement error");
            },
        );
    };
    window.addTermNodeToDatabase = function(termObject) {
        console.log(termObject);
        var cypherStatement = "CREATE (e:Term {";
        cypherStatementEnd = "}) RETURN e";
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
        console.log("Cypher statement: " + cypherStatement);
        runCypherStatement(cypherStatement);
    };
};
