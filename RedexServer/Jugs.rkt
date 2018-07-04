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
; A jug (x v v) x: name of the jug, first v: capacity of the jug, second v: content of the jug

(define-language jugs
  (p (jugs (x v v) (x v v)))
  ; (e (set! x e) (+ e e) x v)
  (v number)
  (x variable))
  ; (pc (jugs (x v v) ...)))
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

    ; (-->
    ;     (jugs (x_1 v_1 v_11) ... (x_i v_i number_ii) (x_2 v_2 v_22) ... (x_j number_j number_jj) (x_3 v_3 v_33) ...)
    ;    ;------------------------------------------------------------------------------- [transfer]
    ;     (jugs (x_1 v_1 v_11) ... (x_i v_i 0) (x_2 v_2 v_22) ... (x_j v_j (+ (term number_ii) (term number_jj))) (x_3 v_3 v_33) ...)
    ; ; (side-condition (not (>= (+ (term number_ii) (term number_jj)) (term number_j))))
    ; transfer)

    ; (-->
    ;     (jugs (x_1 v_1 v_11) (x_2 v_2 v_22))
    ;    ;------------------------------------------------------------------------------- [transferLTR]
    ;     (jugs (x_1 v_1 0) (x_2 v_2 ,(+ (term v_11) (term v_22))))
    ; (side-condition (not (eq? (term v_11) 0)))
    ; ; (side-condition (not (>= (+ (term v_ii) (term v_jj)) (term v_j))))
    ; transferLTR) 

    (-->
        (jugs (x_1 v_1 v_11) (x_2 v_2 v_22))
       ;------------------------------------------------------------------------------- [transferRTL]
        (jugs (x_1 v_1 ,(min (term v_1) (+ (term v_11) (term v_22)))) (x_2 v_2 ,(- (term v_22) (min (term v_22) (- (term v_1) (term v_11))))))
    (side-condition (not (eq? (term v_22) 0)))
    ; (side-condition (not (>= (+ (term v_ii) (term v_jj)) (term v_j))))
    transferRTL) 

    ; (-->
    ;     (jugs (x_1 v_1 v_11) (x_2 v_2 v_22))
    ;    ;------------------------------------------------------------------------------- [transferRTL]
    ;     (jugs (x_1 v_1 ,(min (term v_1) (+ (term v_11) (term v_22)))) (x_2 v_2 ,(max (`0) (- (term v_22) (- (term v_1) (term v_11))))))
    ; (side-condition (not (eq? (term v_22) 0)))
    ; ; (side-condition (not (>= (+ (term v_ii) (term v_jj)) (term v_j))))
    ; transferRTL) 

    (-->
        (jugs (x_1 v_1 v_11) ... (x_i v_i v_ii) (x_2 v_2 v_22) ...)
       ;------------------------------------------------------------------------------- [empty]
        (jugs (x_1 v_1 v_11) ... (x_i v_i 0) (x_2 v_2 v_22) ...)
    (side-condition (not (eq? (term v_ii) 0)))
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
