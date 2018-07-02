#lang racket
;  ________                    .__      __________           .___             
; /  _____/___________  ______ |  |__   \______   \ ____   __| _/____ ___  ___
;/   \  __\_  __ \__  \ \____ \|  |  \   |       _// __ \ / __ |/ __ \\  \/  /
;\    \_\  \  | \// __ \|  |_> >   Y  \  |    |   \  ___// /_/ \  ___/ >    < 
; \______  /__|  (____  /   __/|___|  /  |____|_  /\___  >____ |\___  >__/\_ \
;        \/           \/|__|        \/          \/     \/     \/    \/      \/
;
; Example to show how to use GraphRedex. 
; This is an adapted version of the threads example from the plt-redex example repository
;
(require redex)
(require graph)
(require "RedexGraph.rkt")

;
; The threadslock language.
; Programs p consists of a store which are key-value pairs and zero or more threads
; Each thread is an expression se
; "getlock x n e" tries to get the lock named x before executing e. This succeeds only if x is worth 0. Taking the lock x puts n in variable x.
; "releaselock x n e" tries to release the lock named x before executing e. This succeeds only if x is worth n. Releasing the lock x puts 0 in variable x.
; The "start" marker at the start of each thread is there to ensure that threads execute their outmost instructions first.
; 
(define-language threadslock
  (p ((store (x v) ...) (threads e ...)))
  (se (start e))
  (e (getlock x v e) (releaselock x v e) x v)
  (v number)
  (x variable)
  (pc ((store (x v) ...) tc))
  (tc (threads se ... ec se ...))
  (ec (getlock variable number ec) (releaselock variable number ec) hole))

(define reductions
  (reduction-relation
   threadslock

   (-->
        ((store (x_1 v_1) ... (x_i 0) (x_2 v_2) ...) (in-hole tc_1 (start (getlock x_i v_new e_1))))
       ;------------------------------------------------------------------------------- [getlock]
          ((store (x_1 v_1) ... (x_i v_new) (x_2 v_2) ...) (in-hole tc_1 (start e_1)))
        
    getlock)

   (-->
        ((store (x_1 v_1) ... (x_i v_lock) (x_2 v_2) ...) (in-hole tc_1 (start (releaselock x_i v_lock e_1))))
       ;------------------------------------------------------------------------------- [releaselock]
          ((store (x_1 v_1) ... (x_i 0) (x_2 v_2) ...) (in-hole tc_1 (start e_1)))
        
    releaselock)))


;
; Translation function to expose certain information of the redex model as attributes for the nodes
;
(define (term->kv exp)
  (match exp
    [`((store (x ,x) (y ,y)) (threads ,t1 ,t2))
       (list (cons 'x x) (cons 'y y))]))
 
(run-server reductions term->kv)
