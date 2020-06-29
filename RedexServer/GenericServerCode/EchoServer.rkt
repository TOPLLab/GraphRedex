#lang racket
(provide run-echo)
(require redex/reduction-semantics )
(require redex/pict )
(require file/convertible)
(require racket/exn)
(require "./ArangoDB.rkt")



(define (run-echo graphname langname redLimit relation trans read-term)

  (define endtime (+ (current-seconds) 45))

  (define db (arango-new "graphredex-data" graphname))

  (define (expr->string e) (format "~s" e))

  (define (set-pictures) 
    ; TODO only run when not exists

    ; TODO: see if this can be made cleaner
    (define (safe-render name)
      (render-reduction-relation-rules `(,name))
      (call/cc 
       (λ (cont) 
         (call-with-exception-handler 
          (λ x (cont #f))
          (λ () (pict->svg (render-reduction-relation relation))))))
      )
    (define (name-render-pair name)
      `(,name . ,(safe-render name))
      )

    (define (make-hash-of-rule-render)
      (make-hash
       (map
        name-render-pair
        (if (reduction-relation? relation)
            (reduction-relation->rule-names relation)
            '()
            )
        )
       )
      )

    (arango-qry
     db
     "UPDATE DOCUMENT(CONCAT('languages/',@langkey)) WITH { rules: @rules } IN languages"
     `#hash(
            (langkey . ,langname)
            (rules . ,(make-hash-of-rule-render))
            ))
      
    )


  (define (node-done? term)
    (let ([res 
           (hash-ref 
            (arango-qry db
                        "FOR doc IN @@tcol 
                           FILTER doc.term == @term AND doc._expanded==true 
                           LIMIT 1
                           RETURN doc._id"
                        `#hash((term . ,(expr->string term) )   )
                        #t
                        )
            'result)])
      (> (length res) 0)
      )
    )


  (define (pict->svg pict)
    (bytes->string/utf-8 (convert pict 'svg-bytes))
    )

  (define (secure-trans t)
    (with-handlers ([ exn:fail? (λ e (list (cons 'error_in_term->kv (exn->string e))))]) (trans t))

    )

  (define (make-node-data term)
    (let* (
           [user-data (secure-trans term)]
           [full-data (make-hash
                       (append
                        `((term . ,(expr->string term)))
                        user-data)) ]
           )
      ; Convert pict to svg is it exists
      (cond [(hash-has-key? full-data '_pict)
             (hash-set! full-data '_pict
                        (pict->svg (hash-ref full-data '_pict)))
             
             ])
      
      ; return data
      full-data
      )
    )


  (define (process term-stream reductions-left)
    (cond
      ; Stop if out of runs
      [(= 0 reductions-left) 
       (fprintf (current-error-port) "\nOUT OF REDUCTIONS\n")]

      ; Stop if out of time
      [(> (current-seconds) endtime)
       (fprintf (current-error-port) "\nOUT OF TIME ~a s over time ~a left\n" (- (current-seconds) endtime) reductions-left)]

      ; Stop if no more terms to proccess
      [(stream-empty? term-stream)
       (fprintf (current-error-port) "\nDONE (~a reductions left)\n" reductions-left)]

      ; Skip is tern us already processed
      [(node-done? (stream-first term-stream))
       ;(fprintf (current-error-port) "\nSKIPPED\n")
       (process (stream-rest term-stream) reductions-left)
       ]

      ; Expand the term at the head of the stream
      ; TODO: remove positive?
      [(positive? reductions-left)
       (let*
           (
            (term (stream-first  term-stream))
            (next-terms (apply-reduction-relation/tag-with-names relation term))
            )
         (arango-make-node db (make-node-data term) #t (zero? (length next-terms)))

         (for ([x next-terms])
           (match x [`(,rel ,term2)
                     ;(fprintf (current-error-port) "\nAdded: ~a -[reduces:~a]-> ~a\n" term rel term2)
                     (arango-make-node db (make-node-data term2) #f #f)
                     (arango-make-edge db (expr->string term) (expr->string term2) rel)
                     ])
           )
         (process 
          (stream-append
           (stream-rest term-stream)
           (map (lambda (x) (car (cdr x))) next-terms)) 
          (- reductions-left 1))
         )]
      )

    )





  ; "main"
  (let ([term (read-term)])
    (set-pictures)
    (arango-make-node db (make-node-data term) #f #f )
    (display (car (arango-lookup db (expr->string term))))
    (process (stream term) redLimit)
    )

  )



