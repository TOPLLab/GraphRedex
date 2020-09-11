#!/usr/bin/env node
// var debug = require('debug')('my-application');
require("./dist/SetupAndClear.js")
    .prepare()
    .catch((e) => {
        if (e.response) {
            console.log(
                "error",
                e.response.body,
                e.response.request.path,
                e.response.request.method,
                "error",
            );
        } else {
            console.log("bad",e.code);
            switch (e.code) {
                case "ECONNREFUSED":
                    console.log("Possible reasons:");
                    console.log("Is ArangoDB up?");
                    console.info("  systemctl start arangodb3.service");
                    break;
                default:
                    console.log(e.code);
            }
        }
        process.exit(1);
    })
    .then(() => {
        console.log("Database has been set up");
    });
