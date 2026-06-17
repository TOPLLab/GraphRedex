{ pkgs ? import <nixpkgs> { }, stdenv ? pkgs.stdenv }:
let
  node = pkgs.nodejs_20;
  yarn = pkgs.yarn.override { nodejs = node; };
in
pkgs.mkShell {
  buildInputs = [
    pkgs.nodePackages.prettier
    pkgs.racket
    pkgs.coreutils
    pkgs.entr
    pkgs.docker
    pkgs.curl
    node
    yarn
    pkgs.xvfb-run
  ];
  shellHook = ''
    echo "settup env"
    curl -s -I "localhost:8529" | grep 'Server: *ArangoDB' 2>/dev/null >/dev/null || (
        echo "Start arango"
        docker run -d --name "graphredex-db" -p 8529:8529 -e ARANGO_ROOT_PASSWORD="yolo" arangodb/arangodb:3.7.2 || docker start graphredex-db
        echo "wait..."
        sleep 5s
    )
    export ARANGO_ROOT_PASSWORD="yolo"
  '';
}
