#lang racket
(require redex/reduction-semantics)
(require redex/pict)
(provide reductions term->kv)
; A simple nodeterministic model for showcasing non-deterministic debugging.
; Christophe.Scholliers@UGent.be


(define-language Amb
  (e (amb e ...)
     (+ e ...)
     number)
  (E (+ number ... E e ...) hole))


(define (term->kv exp)
  `(
    (_pict . ,(render-term/pretty-write Amb exp))
    (type . ,(match exp [`(amb ,e ...) "ambigous"] [`(+ ,e ...) "addition"] [_ "final"]) )
    )
  )


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






