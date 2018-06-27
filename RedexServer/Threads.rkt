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
; The threads language 
; Programs p consists of a store which are key-value pairs and zero or more threads
; Each thread is an expression e
; 
(define-language threads
  (p ((store (x v) ...) (threads e ...)))
  (e (set! x e) (+ e e) x v)
  (v number)
  (x variable)
  (pc ((store (x v) ...) tc))
  (tc (threads e ... ec e ...))
  (ec (set! variable ec) (+ ec e) (+ v ec) hole))

(define reductions
  (reduction-relation
   threads

   (-->       (in-hole pc_1  (+ number_1 number_2))
        ;------------------------------------------------------ [sum]
          (in-hole pc_1 ,(+ (term number_1) (term number_2)))
    sum)

   (-->
          ((store  (x_1 v_1)... (x_i v_i) (x_2 v_2) ...) (in-hole tc_1 x_i))
        ;-------------------------------------------------------------------- [deref]
          ((store  (x_1 v_1) ...  (x_i v_i) afters ...) (in-hole tc_1 v_i))
        
   deref)
   
   (-->
        ((store (x_1 v_1) ... (x_i v) (x_2 v_2) ...) (in-hole tc_1 (set! x_i v_new)))
       ;------------------------------------------------------------------------------- [set!]
          ((store (x_1 v_1) ... (x_i v_new) (x_2 v_2) ...) (in-hole tc_1 v_new))
        
    set!)))


;
; Translation function to expose certain information of the redex model as attributes for the nodes
;
(define (term->kv exp)
  (match exp
    [`((store (x ,x)) (threads ,t1 ,t2))
       (list (cons 'x x) )]))
 

(run-server reductions term->kv)