#!/usr/bin/sh
# Name:
# By Robbert Gurdeep Singh
################################################################################
echo "((store (x 1)) (threads (set! x (+ x -1)) (set! x (+ x 1))))" | LC_ALL=C racket RedexServer/Threads.rkt
