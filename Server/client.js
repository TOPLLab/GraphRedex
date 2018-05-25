function connectToNeo4jDatabase() {
    var authToken = neo4j.v1.auth.basic("neo4j", "neo4j-js-password");
    console.log(authToken);
    var driver = neo4j.v1.driver("bolt://localhost", authToken, {
        encrypted: false,
    });
    var session = driver.session();
}

function runCypherStatement(statement) {
    session.run(statement, parameters).subscribe({
        onNext: function(record) {
            // On receipt of RECORD
            var tr = document.createElement("tr");
            record.forEach(function(value) {
                var td = document.createElement("td");
                td.appendChild(document.createTextNode(value));
                tr.appendChild(td);
            });
            table.appendChild(tr);
        },
        onCompleted: function(metadata) {},
    });
}

window.onload = function() {
    var sock = new WebSocket("ws://localhost:8081/");
    var messagecounter = 0;
    sock.onopen = function() {
        console.log("open", arguments);
    };
    sock.onmessage = function(e) {
        var obj = JSON.parse(e.data);
        for (var i in obj) {
            console.log(obj[i]);
            for (k in obj[i]) {
                console.log("K: " + k + " V: " + obj[i][k]);
                runCypherStatement(
                    "CREATE (e:Term {" + k + ": " + obj[i][k] + "}) RETURN e",
                );
            }
        }

        //console.log('message', e.data);
    };
    sock.onclose = function() {
        console.log("close", arguments);
    };
    window.send = function() {
        var term = document.getElementById("Term").value;
        sock.send(term);
    };
};
