module.exports = function(grunt) {
    "use strict";

    grunt.initConfig({
        ts: {
            app: {
                tsconfig: "./tsconfig.json",
            },
            frontend: {
                tsconfig: "./tsconfig-frontend.json",
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
        watch: {
            ts: {
                files: ["src/**/*.ts"],
                tasks: ["ts:app", "tslint"],
            },
            frontend: {
                files: ["public/js/**/*.ts"],
                tasks: ["ts:frontend"],
            },
        },
    });

    grunt.loadNpmTasks("grunt-contrib-watch");
    grunt.loadNpmTasks("grunt-ts");
    grunt.loadNpmTasks("grunt-tslint");

    grunt.registerTask("default", ["ts", "tslint"]);
};
