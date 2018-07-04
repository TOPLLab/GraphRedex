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
(require "./GenericServerCode/RedexGraph.rkt")

;
; The threads language 
; Programs p consists of a store which are key-value pairs and zero or more threads
; Each thread is an expression e
; 
(define-language jugs
  (p (jugs (x v v) ...))
  ; (e (set! x e) (+ e e) x v)
  (v number)
  (x variable)
  (pc (jugs (x v v) ...)))
  ; (tc (threads e ... ec e ...))
  ; (ec (set! variable ec) (+ ec e) (+ v ec) hole))

(define reductions
  (reduction-relation
   jugs

   ; (-->       (in-hole pc_1  (+ number_1 number_2))
   ;      ;------------------------------------------------------ [sum]
   ;        (in-hole pc_1 ,(+ (term number_1) (term number_2)))
   ;  sum)

   ; (-->
   ;        ((store  (x_1 v_1)... (x_i v_i) (x_2 v_2) ...) (in-hole tc_1 x_i))
   ;      ;-------------------------------------------------------------------- [deref]
   ;        ((store  (x_1 v_1) ...  (x_i v_i) (x_2 v_2) ...) (in-hole tc_1 v_i))
        
   ; deref)

    (-->
        (jugs (x_1 v_1 v_11) ... (x_i v_i v_ii) (x_2 v_2 v_22) ...)
       ;------------------------------------------------------------------------------- [empty]
        (jugs (x_1 v_1 v_11) ... (x_i v_i 0) (x_2 v_2 v_22) ...)
        
    empty) 

    (-->
        (jugs (x_1 v_1 v_11) ... (x_i v_i v_ii) (x_2 v_2 v_22) ...)
       ;------------------------------------------------------------------------------- [fill]
        (jugs (x_1 v_1 v_11) ... (x_i v_i v_i) (x_2 v_2 v_22) ...)
        
    fill)))


;
; Translation function to expose certain information of the redex model as attributes for the nodes
;
(define (term->kv exp)
  (match exp
    [`(jugs (x ,x ,a) (y ,z ,t))
       (list (cons 'x a) (cons 'y t) )]))
 
(run-server reductions term->kv)
