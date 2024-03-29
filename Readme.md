# GraphRedex

GraphRedex an open-source tool that empowers language designers to interactively
explore their reduction graphs. From a PLT Redex language definition and a
program in the language, our tool creates an interactive reduction graph. Using
GraphRedex for visualizing reduction graphs has three main benefits. First, a
global exploration mode allows users to obtain a bird's-eye overview of the
reduction graph and learn its high level workings. Second, a local exploration
mode lets the programmer closely interact with the reduction rules. Third, a
query interface allows the programmer to filter out or highlight specific
features of the reduction graph.

## Running the server

Clone the repository and run the following in the root dir.

```bash
./run -c -b
```

Try `./run -h` to see more options.

A web server will be live at `http://localhost:3000`. The default login is
`demo:demo`. The default password for the `demo` user can be set in
`~/graphredex-login.json` (as a hashed password). The contents of this file can
be generated be executing the following in the root of this repo:

```bash
node <<<'require("./APIServer/dist/password.js").hashPassword("PASSWORD").then(x=>console.log(JSON.stringify(x)))'
```

## Known issues when uploading a new language

```racket
(require redex)
```

Should be replaced by

```racket
(require redex/reduction-semantics)
```

All other GUI `require`s should be removed or replaced with a variant that does
not require a display.

This step is not needed when using docker.

## Installation

### Dependencies

-   [node](https://nodejs.org/en/)
    -   OSX: `brew install node`
    -   Arch: `pacman -S nodejs` (community/nodejs)
-   [yarn](https://yarnpkg.com/en/)
    -   OSX `brew install yarn`
    -   Arch: `pacman -S yarn` (community/yarn)
-   [coreutils](https://www.gnu.org/software/coreutils/coreutils.html)
    -   OSX: `brew install coreutils`
    -   Arch: `pacman -S coreutils` (core/coreutils)
-   [ArangoDB](https://www.arangodb.com/) (see below for setting it up)
    -   OSX: `brew install arangodb`
    -   Arch: install `aur/arangodb` from the AUR

### AragnoDB setup (database)

GraphRedex uses a graph database called [ArangoDB](https://www.arangodb.com/).
We use version `3.5.2` but any version above `3.5` should be ok.

**OSX** users can install it with brew:

```
brew install arangodb
/usr/local/opt/arangodb/sbin/arangod &
```

**Arch Linux** users can install the `arangodb3` package form the AUR and start
or enable the service using `systemctl start arangodb3.service`.

#### Configuration of ArangoDB

Once the database is installed it needs to be initialized with an initial
database and two users. Configuration of the database is browser based, the
default address is http://127.0.0.1:8529. The default password for the `root`
user is blank `""`.

#### Creating users

In order for GraphRedex to work it needs to have two users in the system called
`graphredex-qry` and `graphredex`. The user `graphredex` has read and write
access while the user `graphredex-qry` only has read access.

-   In the browser, go to the tab "Users", and create a user called
    `graphredex-qry` with password `graphredex-qry`.
-   In the browser, go to the tab "Users", and create a user called `graphredex`
    with password `graphredex`.

##### Creating the database

Go to the tab "Databases" and make a database called `graphredex-data` make sure
to select `graphredex` as the owner.

##### Setting the permissions

Now that the database is created we still need to make sure that the permissions
of the users are correct:

Go to the Users tab and select permissions tab:

-   In Users/graphredex in the permissions tab (these should normally already be
    set):

    -   give `graphredex` administrative access to the database
        `graphredex-data`
    -   give `graphredex` read/write access to all collections
        -   Click on `graphredex-data`
        -   Select "Read/Write" access on the line with `*`

-   In Users/graphredex-qry in the permissions tab:
    -   give `graphredex-qry` access access on the `graphredex-data` database
    -   give `graphredex-qry` read access to all collections in that database
        -   Click on `graphredex-data`
        -   Select "Read only" access on the line with `*`

## Advanced options

### Running code samples with docker

To run code we receive as input in a docker container, set the
`GRAPHREDEX_DOCKER` environment variable to `1`.

```bash
GRAPHREDEX_DOCKER=1 ./run  -c
```

_Note_: read [RedexServer/README.md](RedexServer/README.md) for security
guidelines.

### Set reduction limit

You can set a limit on the number of reductions preformed by default by setting
the environment variable `GRAPHREDEX_COUNT` (default: 1000).

```bash
GRAPHREDEX_COUNT=100 ./run -c
```

### Allow the use `(require redex)`

This allows you to use
[xvfb](https://www.x.org/releases/X11R7.7/doc/man/man1/Xvfb.1.xhtml). To emulate
having a screen. This is enabled by default in de docker version and comes with
a performance hit.

```bash
GRAPHREDEX_XVFB=1 ./run  -c
```

For the moment, you will need to pach xvfb-run with
[this path](RedexServer/xvfb.patch) to get it to work.

### Setting the root password of AranogDB for auto setup

GraphRedex has an auto-setup feature. This proccess creates the required
database and users. For this to work the root password of arangoDB is needed.
You will be prompted for this if needed. For usage in scripts, you can also set
the environment varaiable `ARANGO_ROOT_PASSWORD` to the passsword of the root
user of the DB. This varaiable not be passed to ran code.

### Using your own plugins from the query pannel

If you create `APIServer/public/dist/Plugin.js` with an AMD style module
definition, the search panel will show an extra button named "custom". Clicking
this button will invoke the `exports.default` function. The two arguments
supplied are:

1. The result from the AQL query
2. A graphredex object with following properties
    - `render(graph)`
    - `highlight(graph)`
    - `expand({_id: nodeid})`

The function should return true on success.

Example content of `Plugin.js`

```js
define(["exports"], function (exports) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    exports.default = (graphredex, result) => {
        // This function will get the result
        console.log("Plugin GR got:", graphredex);
        console.log("Plugin  R got:", result);

        for (let e of result[0].nodes) {
            e._formatted = e._key;
        }

        // continue rendering
        return graphredex.render(result);
    };
    // Comment the line below to enable this plugin
    // exports.default = null;
});
```

_Tip:_ this file can be autogenerated by creating `Plugin.ts` in
`APIServer/public/js` and exporting a default funciton with TypeScript.

# Citing this

Did you use GraphRedex in your research? Please cite the following:

```bibtex
@article{https://doi.org/10.1002/spe.2959,
    author = {Gurdeep Singh, Robbert and Scholliers, Christophe},
    title = {GraphRedex: Look at your research},
    journal = {Software: Practice and Experience},
    volume = {51},
    number = {6},
    pages = {1322-1351},
    keywords = {operational semantics, PLT Redex, semantics engineering, state explosion, tooling, visualization},
    doi = {https://doi.org/10.1002/spe.2959},
    url = {https://onlinelibrary.wiley.com/doi/abs/10.1002/spe.2959},
    eprint = {https://onlinelibrary.wiley.com/doi/pdf/10.1002/spe.2959},
    abstract = {Abstract A significant aspect of designing new programming languages is to define their operational semantics. Working with a pen and paper version of such a semantics is notoriously difficult. For this reason, tools for computer aided semantics engineering were created. Many of these tools allow programmers to execute their language's operational semantics. An executable semantics makes it easier to verify whether the execution of a program leads to the desired result. When a program exhibits unexpected behavior, the programmer can consult the reduction graph to see what went wrong. Unfortunately, visualization of these graphs is currently not well-supported by most tools. Consequently, the comprehension of errors remains challenging. In this article, we present GraphRedex an open-source tool that empowers language designers to interactively explore their reduction graphs, offering three main benefits. First, a global exploration mode allows users to obtain a bird's-eye overview of the reduction graph and learn its high level workings. Second, a local exploration mode lets the programmer closely interact with the individual reduction rules. Third, our query interface allows the programmer to filter out and highlight specific regions of the reduction graph. We evaluated our tool by carrying out a user study showing that participants comprehend programs on average twice as fast while being able to answer questions more accurately. Finally, we demonstrate how GraphRedex helps to understand the semantics of two published works. Exploration of the semantics with GraphRedex unveiled an issue in one of the implementations of these works, which the author confirmed.},
    year = {2021}
}
```
