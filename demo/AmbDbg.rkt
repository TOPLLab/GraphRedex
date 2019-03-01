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

(define (amb->kv exp)
  (match exp
    [(list 'amb e ...) (list (cons 'type "ambigous") (cons 'fixed (= 1 (length e))) )]
    [(list '+ e ...) (list (cons 'type "addition") )]
    [_ (list (cons 'type "final") (cons 'fixed #t) )]
  ))
 

(define amb-red
  (reduction-relation
   Amb
   #:domain e
   (--> (in-hole E (+ number ...))
        (in-hole E ,(apply + (term (number ...))))
        "add")
   (--> (in-hole E (amb e_x ... e_2  e_y ...))
        (in-hole E e_2)
        "amb")))

(define-extended-language Debug_Amb Amb
  (state step pause)
  (debugger (debug state e)))

(define-judgment-form Debug_Amb
    #:mode     (dstep I   O)
    #:contract (dstep debugger any)
   [ 
       (where (e_0 ... e_x e_n ...) ,(apply-reduction-relation amb-red (term e)))
     --------------------------------------------------------------------------------------------- "Amb-Resume"
       (dstep (debug step e)  (debug pause e_x))
              
   ]
   [ 
     --------------------------------------------------------------------------------------------- "Amb-Step"
       (dstep (debug pause e)  (debug step e))
              
   ])

(define reductions dstep)
(define (term->kv exp)
  (match exp [(list 'debug at normexp)
              (append `(
                (action . (unquote (format "~s" at)))
                ) (amb->kv normexp))
              ]) 

  )





