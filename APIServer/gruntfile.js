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
                tasks: ["ts:frontend"],
            },
        },
    });

    grunt.loadNpmTasks("grunt-contrib-watch");
    grunt.loadNpmTasks("grunt-ts");
    grunt.loadNpmTasks("grunt-tslint");
    grunt.loadNpmTasks("grunt-contrib-less");

    grunt.registerTask("default", ["ts", "tslint", "less"]);
};
