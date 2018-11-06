#!/usr/bin/env node
//var debug = require('debug')('my-application');
var express = require("express");

var path = require("path");
var app = require("./main.js");
let public = path.join(__dirname, "public");

app.set("port", process.env.PORT || 3000);

app.use("/", express.static(public));
app.get("/", (req, res) => {
    res.sendFile(path.join(public, "index.html"));
});

var server = app.listen(app.get("port"), function() {
    console.log("Express server listening on port " + server.address().port);
});
