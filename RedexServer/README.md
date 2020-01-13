# Racket / Redex part

This directory contains the Redex backend for the server

## Usage

`run.sh` should be called with 5 arguments:

1. "`DebuggerServer`" | "`EchoServer`"
2. "`run-echo`"
3. The base language file (likely something with filename PLTGraphRedex.rkt)
4. `language._key`
5. Depth limit

## Modes of operation

The default mode of operation is execution in the current environment.
Alternatively the program can also be run in a docker container.

To run in a docker container, set the environment variable `GRAPHREDEX_DOCKER` to `1`.
_Hint_:This can also be done when executing the API server.

The docker image is assumed to be named `graphredex/racket`

```
docker build -t graphredex/racket -f=racket.DockerFile .
```

-   ARANGO_SERVER="172.17.0.1"
-   ! set `endpoint = tcp://0.0.0.0:8529` in `/etc/arangodb3/arangod.conf`

## Env vars

-   `ARANGO_SERVER` The IP adress of the server, as accesible when running
    -   Whitin docker this is likely `172.17.0.1`
    -   Default: `127.0.0.1`
-   `ARANGO_PORT` port aranogoDB is running on
    -   Default: `8529`
-   `GRAPHREDEX_UNSAFE` set to `1` to automaticly log in with `demo:demo`
    -   Default: ``

## security

The docker only needs to acces port 8529 on the machine that is running it.
The following `iptables` rules can be used to restrict docker.
Replace `172.17.0.1/16` with the `bip` in `/etc/docker/daemon.json`

```bash
sudo iptables -A INPUT -i docker0 -p tcp -d 172.17.0.1/16 --dport 8529 -j ACCEPT
sudo iptables -A INPUT -i docker0 -j DROP
```

_ProTip_: to remove these `iptable` rules, repalce `-A` with `-D`.
