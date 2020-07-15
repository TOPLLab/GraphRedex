#lang racket
(require redex/reduction-semantics)
(require redex/pict)
(provide reductions read-term term->kv)
; A simple nodeterministic model for showcasing non-determinism.
; With a simple parser to demo parsing
; Christophe.Scholliers@UGent.be and Robbert.GurdeepSingh@UGent.be


(define-language Amb
  (e (amb e ...)
     (+ e ...)
     number)
  (E (+ number ... E e ...) hole))




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



;; Parsing
;; (+ 1 2 (3 4 5) 6) ⇒ (+ 1 2 (amb 3 4 5) 6)
(define (parse term)
  (match term
    [`(+ ,a ... ) (cons '+ (map parse a))]
    [_ #:when (number? term) term ]
    [_ (cons 'amb (map parse term))])
  )

(define (unparse term)
  (match term
    [`(amb ,a ... ) (map unparse a)]
    [`(+ ,a ... ) (cons '+ (map unparse a))]
    [_ #:when (number? term) term ]
    [_ (cons 'amb (map unparse term))])
  )


; GraphRedex specific functions
(define (read-term) (parse (read)) )
(define (term->kv exp)
  `(
    (_formatted . ,(pretty-format (unparse exp) 15))
    (_pict . ,(render-term/pretty-write Amb (unparse exp)))
    (type . ,(match exp [`(amb ,e ...) "ambigous"] [`(+ ,e ...) "addition"] [_ "final"]) )
    )
  )




