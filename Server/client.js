window.onload = function() {
    // var sock = new WebSocket("ws://localhost:8081/test", "subprotocol");
    var sock = new WebSocket("ws://localhost:8081/");
    var messagecounter = 0;
    sock.onopen = function() {
        console.log("open", arguments);
    };
    sock.onmessage = function(e) {
        console.log("message", e.data);
        messagecounter++;
        if (messagecounter == 10) {
            sock.send("goodbye");
        }
    };
    sock.onclose = function() {
        console.log("close", arguments);
    };
    window.send = function() {
        var term = document.getElementById("Term").value;
        sock.send(term);
    };
};
