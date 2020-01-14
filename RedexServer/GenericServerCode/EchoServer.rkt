#lang racket
(provide run-echo)
(require redex/reduction-semantics )
(require file/convertible)
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





(define (run-echo graphname redLimit relation trans read-term)

  (define db (arango-new "graphredex-data" graphname))

  (define (trans->json t ts trans)
    (jsexpr->string
     (make-hash
      (list
       (cons 'from (expr->string t))
       (cons 'next (map trans ts))))))

  (define (expr->string e) (format "~s" e))




  (define (node-done? term)
    (let ([res (hash-ref (arango-qry db
                                     "FOR doc IN @@tcol FILTER doc.term == @term AND doc._expanded==true LIMIT 1 RETURN doc._id"
                                     `#hash((term . ,(expr->string term) )   )
                                     #t
                                     )
                         'result)])
      (> (length res) 0)
      )
    )



  (define (make-node-data term trans)
    (let* (
           [user-data (trans term)]
           [full-data (make-hash
                       (append
                        `((term . ,(expr->string term)))
                        (trans term))) ]
           )
      ; Convert pict to svg is it exists
      (cond [(hash-has-key? full-data '_pict)
             (hash-set! full-data '_pict
                        (bytes->string/utf-8 (convert (hash-ref full-data '_pict) 'svg-bytes)))
             
             ])
      
      ; return data
      full-data
      )
    )


  (define (process term-stream reductions-left relation trans)
    (cond
      ; Stop if out of runs
      [(= 0 reductions-left) (fprintf (current-error-port) "\nOUT OF REDUCTIONS\n")]

      ; Stop if no more terms to proccess
      [(stream-empty? term-stream) (fprintf (current-error-port) "\nDONE (~a reductions left)\n" reductions-left)]

      ; Skip is tern us already processed
      [(node-done? (stream-first term-stream))
       ;(fprintf (current-error-port) "\nSKIPPED\n")
       (process (stream-rest term-stream) reductions-left relation trans)
       ]

      ; Expand the term at the head of the stream
      ; TODO: remove positive?
      [(positive? reductions-left)
       (let*
           (
            (term (stream-first  term-stream))
            (next-terms (apply-reduction-relation/tag-with-names relation term))
            )
         (arango-make-node db (make-node-data term trans) #t (zero? (length next-terms)))

         (for ([x next-terms])
           (match x [`(,rel ,term2)
                     ;(fprintf (current-error-port) "\nAdded: ~a -[reduces:~a]-> ~a\n" term rel term2)
                     (arango-make-node db (make-node-data term2 trans) #f #f)
                     (arango-make-edge db (expr->string term) (expr->string term2) rel)
                     ])
           )
         (process (stream-append (stream-rest term-stream) (map (lambda (x) (car (cdr x))) next-terms)) (- reductions-left 1) relation trans)
         )]
      )

    )





  (let*
      (
       (term (read-term))
       )
    (arango-make-node db (make-node-data term trans) #f #f )
    (display (car (arango-lookup db (expr->string term))))
    (process (stream term) redLimit relation trans)
    )

  )



