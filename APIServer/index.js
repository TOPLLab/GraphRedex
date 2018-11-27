#!/usr/bin/env node
// var debug = require('debug')('my-application');

if (process.argv.length != 3) {
    console.error("Incorect argument");
    process.exit();
}

const express = require("express");

const path = require("path");
const public = path.join(__dirname, "public");

const datadir = process.argv[2];
console.log(process.argv);

require("./dist/Server.js")
    .default.bootstrap(datadir)
    .then((server) => {
        server.app.set("port", process.env.PORT || 3000);

        server.app.use("/", express.static(public));
        server.app.get("/", (req, res) => {
            res.sendFile(path.join(public, "index.html"));
        });
        const s = server.app.listen(server.app.get("port"), function() {
            console.log("Express server listening on port " + s.address().port);
            console.log("Datadir is " + datadir);
        });
    })
    .catch((e) => {
        console.log(e.code);
        switch (e.code) {
            case "ECONNREFUSED":
                console.log("Possible reasons:");
                console.log("Is ArangoDB up?");
                console.info("  systemctl start arangodb3.service");
                break;
            default:
                console.log(e.code);
        }
    });
