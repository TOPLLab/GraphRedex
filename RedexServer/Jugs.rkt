#lang racket
;  ________                    .__      __________           .___             
; /  _____/___________  ______ |  |__   \______   \ ____   __| _/____ ___  ___
;/   \  __\_  __ \__  \ \____ \|  |  \   |       _// __ \ / __ |/ __ \\  \/  /
;\    \_\  \  | \// __ \|  |_> >   Y  \  |    |   \  ___// /_/ \  ___/ >    < 
; \______  /__|  (____  /   __/|___|  /  |____|_  /\___  >____ |\___  >__/\_ \
;        \/           \/|__|        \/          \/     \/     \/    \/      \/
;
; Example to show how to use GraphRedex. 
;
(require redex/reduction-semantics)
;(require graph)

;
; The jugs language 
; Programs p consists of two jugs
; 
; Jug: (x v v) where 
;   x: name of the jug
;   first v: capacity of the jug
;   second v: content of the jug
;
; Operations:
;   - Emptying a jug (but not if it's already empty)
;   - Filling a jug to its maximal capacity (but not if it's already full)
;   - Transfering the content of a jug into another (but not if the source jug is empty) (the source jug retains the leftover if not all of its content can be transferred)
;       For the moment, there is only a transfer rule from jugs on the right (in the program term) to jugs on the left
;
; Standard example: Starting from (jugs (x 5 0) (y 3 0)), how to get to (jugs (x 5 4) (y 3 0))?

(define-language jugs
  (p (jugs (x v v) (x v v)))
  (v number)
  (x variable))

(define reductions
  (reduction-relation
   jugs

    (-->
        (jugs (x_1 v_1 v_11) (x_2 v_2 v_22))
       ;------------------------------------------------------------------------------- [transferRTL]
        (jugs (x_1 v_1 ,(min (term v_1) (+ (term v_11) (term v_22)))) (x_2 v_2 ,(- (term v_22) (min (term v_22) (- (term v_1) (term v_11))))))
        (side-condition (and (not (eq? (term v_22) 0)) (not (eq? (term v_1) (term v_11)))))
    transferRTL)

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
        (side-condition (not (eq? (term v_i) (term v_ii))))
    fill)))


;
; Extraction function to expose certain information of the redex model as attributes for the nodes
;
(define (term->kv exp)
  (match exp
    [`(jugs (x ,x ,a) (y ,z ,t))
       (list (cons 'x a) (cons 'y t) )]))
 

(require "./GenericServerCode/EchoServer.rkt")
(run-echo reductions term->kv)
