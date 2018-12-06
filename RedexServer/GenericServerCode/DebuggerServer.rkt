#lang racket
(provide run-echo)
(require redex/reduction-semantics )
(require json)
;(require file/md5)
;(require racket/stream)
(require "./ArangoDB.rkt")



;  ________                    .__      __________           .___
; /  _____/___________  ______ |  |__   \______   \ ____   __| _/____ ___  ___
;/   \  __\_  __ \__  \ \____ \|  |  \   |       _// __ \ / __ |/ __ \\  \/  /
;\    \_\  \  | \// __ \|  |_> >   Y  \  |    |   \  ___// /_/ \  ___/ >    <
; \______  /__|  (____  /   __/|___|  /  |____|_  /\___  >____ |\___  >__/\_ \
;        \/           \/|__|        \/          \/     \/     \/    \/      \/
; Christophe.Scholliers@UGent and Thomas.Dupriez@ens-paris-saclay.fr
;





(define (run-echo graphname relation trans)

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

  (define
    (doDbgNext term)
    (match term [(list bp bc 'pause h '() am tm ta k)  (map
                   (lambda (x) (list (format "~s" x) (list bp bc 'pause h (list (list x)) am tm ta k)))
                   '(Step-Msg-Receiver Step-Future-Resolver Step-Future-Resolution Return-From-Turn-To-Future-Resolution Step-Next-Turn Step-End-Turn Resume-Execution Pause-Execution Stop-Execution)
                   )]
                 [(list bp bc ststst   h c am tm ta k) (list)
                 ])
    )



  (define
    (doApl relation term)
    
     (let ((next-terms (apply-reduction-relation/tag-with-names relation term)))
       (if
         (zero? (length next-terms))
          (map (lambda x (cons #f (car x))) (doDbgNext term))
          (map (lambda x (cons #t (car x))) next-terms)
         )
       )
     )


  (define (run-echo2 terms cnt relation trans)
    (fprintf (current-error-port) "\n (~a reductions left)\n" cnt)
    (cond
      [(= 0 cnt) (fprintf (current-error-port) "\nOUT OF REDUCTIONS\n" cnt)]
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
          (next-terms (doApl relation term))
          )
         (makenode (betterTrans term) #t (zero? (length next-terms)))



         (for
           ([x next-terms])
           (match x [
                     (list real rel term2)
                     ;(fprintf (current-error-port) "\nAdded: ~a -[reduces:~a]-> ~a\n" term rel term2)
                     (makenode (betterTrans term2) #f #f)
                     (makeedge (expr->string term) (expr->string term2) rel real)

                     ])
           )
         (run-echo2 (stream-append 
                        (stream-rest terms) 
                        (map (lambda (x) (car (cdr (cdr x)))) next-terms)
                    ) 
                    (- cnt 1) relation trans)
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
    (run-echo2 (stream term) 1000 relation trans)
    ;TODO set at base
    )

  )



