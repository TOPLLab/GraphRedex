#!/bin/sh
# Name: Create a temp racket file with the absolute path to the language definition
# By Robbert Gurdeep Singh
# Args
# 1: DebuggerServer | EchoServer
# 2: run-echo
# 3: file
# 4: language._key
# 5: limit
################################################################################
# Just to be sure we overwrite potentially sensitive env vars.
export ARANGO_ROOT_PASSWORD=""

TMPFILE=$(mktemp)
trap "rm -f $TMPFILE" EXIT

CURPOS=$(dirname $0)
CURLOC=$(realpath $CURPOS)

if [ "1" = "$GRAPHREDEX_DOCKER" ]; then
    cat >$TMPFILE <<HERE
#lang racket
(require (prefix-in user: (file "/home/runner/data/$(basename $3)")))
(require (file "/home/runner/server/$1.rkt"))
HERE
else
    cat >$TMPFILE <<HERE
#lang racket
(require (prefix-in user: (file "$3")))
(require (file "$CURLOC/GenericServerCode/$1.rkt"))
HERE
fi

cat >>$TMPFILE <<HERE
; Fallback function definition
(define-syntax (fallback-func stx)
  (syntax-case stx ()
    [(_ id fallback)
     (let ([where (identifier-binding #'id)])
       (if where  #'(void) #'(define id fallback)))]))

(fallback-func user:term->kv (lambda (term) '()))
(fallback-func user:read-term read)

($2 "$4" $5 user:reductions user:term->kv user:read-term)
HERE

if [ "1" = "$GRAPHREDEX_DOCKER" ]; then
    docker run \
        -i \
        -e ARANGO_SERVER="$(ip route show dev docker0 | grep -o ' [0-9.]\+ ' | grep -o '[0-9.]\+')" \
        -v $TMPFILE:/home/runner/run.rkt:ro \
        -v $CURLOC/GenericServerCode:/home/runner/server:ro \
        -v $(dirname $3):/home/runner/data:ro \
        --rm \
        graphredex/racket
else
    if [ "1" = "$GRAPHREDEX_XVFB" ]; then
        xvfb-run -- racket $TMPFILE
    else
        racket $TMPFILE
    fi
fi
