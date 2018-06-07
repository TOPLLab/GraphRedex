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
        console.log("obj= " + obj);
        obj = obj.next;
        console.log("obj.next= " + obj);
        for (var i in obj) {
            console.log(obj[i]);
            var cypherStatement = "CREATE (e:Term {";
            cypherStatementEnd = "}) RETURN e";
            var addComma = false;
            for (k in obj[i]) {
                console.log("K: " + k + " V: " + obj[i][k]);
                if (addComma) {
                    cypherStatement = cypherStatement + ", ";
                } else {
                    addComma = true;
                }
                cypherStatement =
                    cypherStatement +
                    k +
                    ": " +
                    valueToStringForCypherStatement(obj[i][k]);
            }
            cypherStatement = cypherStatement + cypherStatementEnd;
            console.log("Cypher statement: " + cypherStatement);
            runCypherStatement(cypherStatement);
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
};
