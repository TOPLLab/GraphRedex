#!/bin/sh
# Name: Start server with tmp directory that is cleared and removed on exit
# By Robbert Gurdeep Singh
################################################################################

if [[ $# -eq 1 ]]; then
set -e
yarn run grunt
fi

MYTMPDIR=$(mktemp -d)
trap "echo STOP;rm -rf $MYTMPDIR" EXIT

mkdir -p "$MYTMPDIR/1/lang/1"
cp  $(dirname $0)"/../RedexServer/Jugs.rkt" "$MYTMPDIR/1/lang/1/main.rkt"
mkdir -p "$MYTMPDIR/1/lang/2"
cp  $(dirname $0)"/../RedexServer/Test.rkt" "$MYTMPDIR/1/lang/2/main.rkt"

./index.js "$MYTMPDIR"
