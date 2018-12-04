#!/bin/sh
# Name: Create a temp racket file with te absolute path to the language definition
# By Robbert Gurdeep Singh
################################################################################

TMPFILE=$(mktemp)
trap "rm -rf $TMPFILE" EXIT

CURPOS=$(dirname $0)
CURLOC=$(realpath $CURPOS)

(echo '#lang racket'
echo '(require (prefix-in user: (file "'$1'")))'
echo '(require (file "'$CURLOC'/GenericServerCode/DebuggerServer.rkt"))'
echo '(run-echo "'$2'" user:reductions user:term->kv)') > $TMPFILE


racket $TMPFILE
