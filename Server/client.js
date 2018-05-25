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
