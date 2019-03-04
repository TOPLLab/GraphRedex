# GraphRedex

GraphRedex is a tool for ...

## Running the server
Clone the repository and run the following in the root dir

```bash
./run -c -b
```

Try `./run -h` to see more options.

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

- [node](https://nodejs.org/en/)
   * OSX: `brew install node`
   * Arch: `pacman -S nodejs` (community/nodejs)
- [yarn](https://yarnpkg.com/en/)
   * OSX `brew install yarn`
   * Arch: `pacman -S yarn` (community/yarn)
- [coreutils](https://www.gnu.org/software/coreutils/coreutils.html)
   * OSX: `brew install coreutils`
   * Arch: `pacman -S coreutils` (core/coreutils)
- [ArangoDB](https://www.arangodb.com/) (see below for setting it up)
   * OSX: `brew install arangodb`
   * Arch: install `aur/arangodb` from the AUR

### AragnoDB setup (database)

GraphRedex uses a graph database called [ArangoDB](https://www.arangodb.com/). 
We use version `3.3.19` but I higher version should also be fine.


**OSX** users can install it with brew:
```
brew install arangodb
/usr/local/opt/arangodb/sbin/arangod &
```

**Arch Linux** users can install the `arangodb3` package form the AUR and
start or enable the service using `systemctl start arangodb3.service`.

#### Configuration of ArangoDB 
Once the database is installed it needs to be initialised with an initial database and two users. 
Configuration of the database is browser based, the default address is http://127.0.0.1:8529.
The default password for the `root` user is blank `""`.


#### Creating users 

In order for GraphRedex to work it needs to have two users in the system called `graphredex-qry` and `graphredex`.
The user `graphredex` has read and write access while the user `graphredex-qry` only has read access. 

- In the browser, go to the tab "Users", and create a user called `graphredex-qry` with password `graphredex-qry`. 
- In the browser, go to the tab "Users", and create a user called `graphredex` with password `graphredex`.

##### Creating the database  

Go to the tab "Databases" and make a database called `graphredex-test` make sure to select `graphredex` as the owner. 

##### Setting the permissions 

Now that the database is created we still need to make sure that the permissions of the users are correct:

Go to the Users tab and select permissions tab: 
- In Users/graphredex in de permissions tab (these should normally already be set):
  * give `graphredex` administrative access to the database `graphredex-test`
  * give `graphredex` read/write access to all collections
    + Click on `graphredex-test`
    + Select Read access on the line with `*`

- In Users/graphredex-qry in de permissions tab:
  * give `graphredex-qry` access access on the `graphredex-test` database
  * give `graphredex-qry` read access to all collections in that databse
    + Click on `graphredex-test`
    + Select Read access on the line with `*`


## Advanced options

### Running code samples with docker

To run code we receive as input in a docker container, set the `GRAPHREDEX_DOCKER` 
environment variable to `1`.

```bash
GRAPHREDEX_DOCKER=1 ./run  -c  
```

*Note*: read [RedexServer/README.md](RedexServer/README.md) for security guidelines.