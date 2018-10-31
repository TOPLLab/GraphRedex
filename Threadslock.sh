#!/bin/sh
# Name:
# By Robbert Gurdeep Singh
################################################################################
echo "((store (x 0) (y 0)) (threads (start (getlock x 1 (getlock y 1 (releaselock y 1 (releaselock x 1 1)))))(start (getlock y 2 (getlock x 2 (releaselock x 2 (releaselock y 2 2)))))))" | LC_ALL=C racket RedexServer/Threadslock.rkt

