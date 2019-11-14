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
console.log("Starting server");

require(process.env.ENV === "DEVELOP" || process.env.GRAPHREDEX_UNSAFE === "YES"
    ? "./dist/ServerNoLogin.js"
    : "./dist/Server.js")
    .default.bootstrap(
        datadir,
        path.join(__dirname, "..", "RedexServer", "run.sh"),
    )
    .catch((e) => {
        console.log("An error occured");
        console.log(e.code);
        switch (e.code) {
            case "ECONNREFUSED":
                console.log("Possible reasons:");
                console.log("Is ArangoDB up?");
                console.info("  systemctl start arangodb3.service");
                break;
            default:
                console.log(e.code || e);
        }
        process.exit(1);
    })
    .then((server) => {
        server.app.set("port", process.env.PORT || 3000);
        server.app.use("/", express.static(public));
        server.app.get("/", (req, res) => {
            res.sendFile(path.join(public, "index.html"));
        });
        const s = server.app
            .listen(server.app.get("port"), function() {
                console.log(
                    "Express server listening on port " + s.address().port,
                );
                console.log("Datadir is " + datadir);
            })
            .on("error", function(err) {
                if (err.errno === "EADDRINUSE") {
                    console.log(
                        "Could not setup server, is port " +
                            server.app.get("port") +
                            " already in use?",
                    );
                } else {
                    throw err;
                }
            });
    })
    .catch((x) => {
        console.log("something went wrong");
        console.error(x);
    });
