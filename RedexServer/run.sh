#!/bin/sh
# Name: Create a temp racket file with te absolute path to the language definition
# By Robbert Gurdeep Singh
# Args
# 1: DebuggerServer | EchoServer
# 2: run-echo
# 3: file
# 4: language._key
# 5: limit
################################################################################

TMPFILE=$(mktemp)
trap "rm -rf $TMPFILE" EXIT

CURPOS=$(dirname $0)
CURLOC=$(realpath $CURPOS)

(echo '#lang racket'
echo '(require (prefix-in user: (file "'$3'")))'
echo '(require (file "'$CURLOC'/GenericServerCode/'$1'.rkt"))'
echo '('$2' "'$4'" '$5' user:reductions user:term->kv)') > $TMPFILE


racket $TMPFILE
