const path = require("path");
module.exports = function (grunt) {
    "use strict";

    grunt.initConfig({
        ts: {
            app: {
                tsconfig: "./tsconfig.json",
            },
        },
        webpack: {
            frontend: {
                mode: "production",
                entry: "./public/js/GraphRedex.ts",
                module: {
                    rules: [
                        {
                            test: /\.tsx?$/,
                            use: {
                                loader: "ts-loader",
                                options: {
                                    configFile: "tsconfig-frontend.json",
                                },
                            },
                            exclude: /node_modules/,
                        },
                        {
                            test: /\.css$/i,
                            use: "raw-loader",
                        },
                        {
                            test: /\.less$/i,
                            use: [
                                { loader: "to-string-loader" },
                                { loader: "css-loader" },
                                { loader: "less-loader" },
                            ],
                        },
                        {
                            test: /\.svg$/i,
                            use: "raw-loader",
                        },
                    ],
                },
                resolve: {
                    extensions: [".tsx", ".ts", ".js"],
                },
                externals: {
                    d3: "d3", // load d3 from cdn if possible
                },
                devtool: "source-map",
                output: {
                    libraryTarget: "amd", // to amd format for loading with requirejs
                    filename: "GraphRedex.js",
                    path: path.resolve(__dirname, "public/dist"),
                },
            },
        },
        tslint: {
            options: {
                configuration: "tslint.json",
            },
            files: {
                src: ["src/**/*.ts"],
            },
        },
        less: {
            production: {
                files: {
                    "public/dist/style.css": "public/less/style.less",
                },
                compress: true,
                sourceMap: true,
            },
        },
        watch: {
            ts: {
                files: ["src/**/*.ts"],
                tasks: ["ts:app", "tslint"],
            },
            less: {
                files: ["public/less/**/*.less", "public/less/*.less"],
                tasks: ["less"],
            },
            frontend: {
                files: ["public/js/**/*.ts", "tsconfig-frontend.json"],
                tasks: ["webpack:frontend"],
            },
        },
    });

    grunt.loadNpmTasks("grunt-contrib-watch");
    grunt.loadNpmTasks("grunt-ts");
    grunt.loadNpmTasks("grunt-tslint");
    grunt.loadNpmTasks("grunt-contrib-less");
    grunt.loadNpmTasks("grunt-webpack");

    grunt.registerTask("default", ["ts", "tslint", "less", "webpack"]);
};
