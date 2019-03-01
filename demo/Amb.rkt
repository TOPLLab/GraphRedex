#lang racket
(require redex/reduction-semantics)
(provide reductions term->kv)
; A simple nodeterministic model for showcasing non-deterministic debugging. 
; Christophe.Scholliers@UGent.be


(define-language Amb
  (e (amb e ...)
     (+ e ...)
     number)
  (E (+ number ... E e ...) hole))


(define (term->kv exp)
  (match exp
    [(list 'amb e ...) (list (cons 'type "ambigous") (cons 'fixed (= 1 (length e))) )]
    [(list '+ e ...) (list (cons 'type "addition") )]
    [_ (list (cons 'type "final") (cons 'fixed #t) )]
  ))
 

(define reductions
  (reduction-relation
   Amb
   #:domain e
   (--> (in-hole E (+ number ...))
        (in-hole E ,(apply + (term (number ...))))
        "add")
   (--> (in-hole E (amb e_x ... e_2  e_y ...))
        (in-hole E e_2)
        "amb")))

;(traces amb-red (term (+ (amb 1 2) (amb 3 4))))






