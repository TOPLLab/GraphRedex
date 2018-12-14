#lang racket
(provide run-echo)
(require redex/reduction-semantics )
(require json)
(require "./ArangoDB.rkt")



;  ________                    .__      __________           .___
; /  _____/___________  ______ |  |__   \______   \ ____   __| _/____ ___  ___
;/   \  __\_  __ \__  \ \____ \|  |  \   |       _// __ \ / __ |/ __ \\  \/  /
;\    \_\  \  | \// __ \|  |_> >   Y  \  |    |   \  ___// /_/ \  ___/ >    <
; \______  /__|  (____  /   __/|___|  /  |____|_  /\___  >____ |\___  >__/\_ \
;        \/           \/|__|        \/          \/     \/     \/    \/      \/
; Christophe.Scholliers@UGent and Thomas.Dupriez@ens-paris-saclay.fr
;





(define (run-echo graphname redLimit relation trans)

  (define-values (arangoGET arangoPOST clearall qry lookup makenode makeedge) (createArango "graphredex-test" graphname))

  (define (trans->json t ts trans)
    (jsexpr->string
      (make-hash
        (list
          (cons 'from (expr->string t))
          (cons 'next (map trans ts))))))

  (define (expr->string e) (format "~s" e))




  (define (node-done? term)
    (let ([res (hash-ref (qry
                           "FOR doc IN @@tcol FILTER doc.term == @term AND doc._expanded==true LIMIT 1 RETURN doc._id"
                           `#hash((term . (unquote (expr->string term) ))   )
                           #t
                           )
                         'result)])
      (> (length res) 0)
      )
    )






  (define (run-echo2 terms cnt relation trans)
    (cond
      [(= 0 cnt) (fprintf (current-error-port) "\nOUT OF REDUCTIONS\n")]
      [(stream-empty? terms) (fprintf (current-error-port) "\nDONE (~a reductions left)\n" cnt)]
      [(node-done? (stream-first terms))
       (fprintf (current-error-port) "\nSKIPPED\n")
       (run-echo2 (stream-rest terms) cnt relation trans)
       ]
      [(positive? cnt)
       (let*
         (
          (term (stream-first  terms))
          (betterTrans (lambda (x) (make-hash (cons (cons 'term (expr->string x)) (trans x)))))
          (next-terms (apply-reduction-relation/tag-with-names relation term))
          (json       (trans->json term next-terms (lambda (x) (match x [(list a b) (make-hash (list (cons 'rule a) (cons 'term (expr->string b)) (cons 'data (make-hash (trans b)))))]))))
          )
         (makenode (betterTrans term) #t (zero? (length next-terms)))

         (for ([x next-terms]) (
                                match x [
                                         (list rel term2)
                                         ;(fprintf (current-error-port) "\nAdded: ~a -[reduces:~a]-> ~a\n" term rel term2)
                                         (makenode (betterTrans term2) #f #f)
                                         (makeedge (expr->string term) (expr->string term2) rel)

                                         ])
              )
         (fprintf (current-error-port) "Added: ~a\n" json)
         (run-echo2 (stream-append (stream-rest terms) (map (lambda (x) (car (cdr x))) next-terms)) (- cnt 1) relation trans)
         )]
      )

    )






  (let*
    (
     (term (read))
     )
    (makenode (hash-set* (make-immutable-hash (trans term))
                         'term (expr->string term)
                         'base #t) 
                         #f
                         #f
                         )
    (display (car (lookup (expr->string term))))
    (run-echo2 (stream term) redLimit relation trans)
    )

  )



