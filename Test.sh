#!/usr/bin/sh
# Name:
# By Robbert Gurdeep Singh
################################################################################
echo "(((func quadruple ((a : int)) : int (block () ((a = (a + a)) (return (a + a)))))     (func double ((a : int)) : int (block () ((a = (a + a)) (return a))))     (func release-lock ((a : (mutex-guard int))) : void (block () ()))     (func release-lock2 ((a : (mutex-guard int))) : int (block () ((return 0) (return 2)))))    (block ((g : (mutex-guard int) = (uninitialized))            (h : (mutex-guard int) = (uninitialized))            (x : int = 1)            (m : (mutex int) = (uninitialized))            (j : join-handle = (uninitialized)))           (            (x = (x + 2))            (x = 4)            (m = (mutex 1))            (j = (fork ((m : (mutex int) = m) (j : join-handle = j) (k : join-handle = (uninitialized)))                       (block ((g : (mutex-guard int) = (uninitialized)) (x : int = (uninitialized)))                              (                               (g = (lock m))                               (call release-lock (g))                               (x = 5)                               (g = (lock m))                               (g = x)                               (x = (call release-lock2 (g)))                               (k = (fork ()                                          (block ((x : int = (uninitialized)) (y : bool = #false))                                                 ((if ((2 + 2 ) == 4) (block () ((x = 7))))                                                  (if y (block () ((y = #true))))))))))))            (g = (lock m))            (join j)            (x = (g + 1))            (h = g)            (x = (call quadruple (x)))            (x = (call double (x))))))"  | LC_ALL=C racket RedexServer/Test.rkt