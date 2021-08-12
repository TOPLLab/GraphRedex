{ pkgs ? import <nixpkgs> { }, stdenv ? pkgs.stdenv }:
let
  xvfb-run-stderr = pkgs.xvfb_run.overrideAttrs (drv: {
    # Patch the wrapped program by appending this to the buildCommand
    buildCommand = drv.buildCommand + ''

        patch $out/bin/.xvfb-run-wrapped << 'HERE'

--- xvfb-run   2019-02-28 06:18:21.000000000 +0000
+++ xvfb-run   2019-03-04 12:37:31.129377336 +0000
@@ -178,7 +178,7 @@

 # Start the command and save its exit status.
 set +e
-DISPLAY=:$SERVERNUM XAUTHORITY=$AUTHFILE "$@" 2>&1
+DISPLAY=:$SERVERNUM XAUTHORITY=$AUTHFILE "$@"
 RETVAL=$?
 set -e
HERE
        '';
  });
  node = pkgs.nodejs-14_x;
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
    xvfb-run-stderr
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
