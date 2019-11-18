#lang racket

(require net/http-client)
(require json)
(provide arango-new
         arango-version
         arango-get
         arango-post
         arango-put
         arango-clear-all
         arango-qry
         arango-lookup
         arango-make-node
         arango-make-edge
         )



(struct arango (send graphname))

; http://localhost:8529/_db/graphredex-data/
(define (arango-new dbname graphname)
  (define (sendArango method qryHead path qry)
    (define-values (status headers in)
      (http-sendrecv (or (getenv "ARANGO_SERVER") "localhost")
                     (~a "/_db/" dbname "/" path)
                     #:ssl? #f
                     #:version "1.1"
                     #:method method
                     #:port (string->number (or (getenv "ARANGO_PORT") "8529"))
                     #:headers qryHead
                     #:data qry))
    (parameterize ([current-output-port (current-error-port)])
      ;(displayln qry)
      ;(displayln qryHead)
      ;(displayln status)
      ;(displayln headers)
      (let ((result (string->jsexpr (port->string in))))
        ;(displayln result)
        (close-input-port in)
        result
        )
      )
    )
  ; Perform login
  (let* (
         (token (hash-ref
                 (sendArango "POST"
                             '( "Content-Type: application/json"
                                "Accept: application/json; charset=UTF-8")
                             "/_open/auth"
                             "{\"username\":\"graphredex\",\"password\":\"graphredex\"}")
                 'jwt
                 ))
         (newHeaders `(
                       "Content-Type: application/json"
                       "Accept: application/json; charset=UTF-8"
                       ,(~a "Authorization: bearer " token)
                       )))

    (arango
     (lambda (method path qry) (sendArango method newHeaders path qry))
     graphname
     )))

(define (arango-version obj) (arango-get obj "_api/version") )


(define (arango-get ar x) ((arango-send ar) "GET" x "") )
(define (arango-post ar x y) ((arango-send ar) "POST" x y) )
(define (arango-put ar x y) ((arango-send ar) "PUT" x y) )


(define (arango-clear-all ar x y)
  ((arango-send ar) "PUT" (~a "_api/collection/" (arango-graphname ar) "-reductions/truncate") "")
  ((arango-send ar) "PUT" (~a "_api/collection/" (arango-graphname ar) "/truncate") "")
  )

(define (arango-qry ar query binds [addTcol #f] [addRcol #f])
  (let* (
         [graphname (arango-graphname ar)]
         (tmpbinds (if addTcol (hash-set binds '@tcol graphname) binds))
         (newbinds (if addRcol (hash-set tmpbinds '@rcol (~a graphname "-reductions")) tmpbinds))
         )
    ((arango-send ar) "POST" "_api/cursor"
                      (jsexpr->string
                       `#hash((query . ,query)
                              (bindVars . ,newbinds))))

    )
  )

(define (arango-lookup ar term)
  (hash-ref
   ((arango-send ar)
    "POST"
    "_api/cursor"
    (jsexpr->string
     `#hash(
            (query . "FOR doc IN @@tcol FILTER doc.term == @term LIMIT 1 RETURN doc._id")
            (bindVars . #hash(
                              (term . ,term )
                              (@tcol . ,(arango-graphname ar)
                                     )))))
    )
   'result)
  )

(define (arango-make-node ar term expanded stuck)
  ((arango-send ar)
   "POST"
   "_api/cursor"
   (jsexpr->string
    `#hash(
           (query .
                  ,(if expanded
                       (~a "LET updatedTerm = MERGE(@term,{\"_expanded\": true, \"_stuck\": " (if stuck "true" "false") "})  UPSERT {term:@term.term} INSERT updatedTerm UPDATE updatedTerm IN @@tcol RETURN NEW._id")
                       "LET updatedTerm = MERGE(@term,{\"_expanded\": false}) UPSERT {term:@term.term} INSERT updatedTerm UPDATE OLD IN @@tcol RETURN NEW._id"))
           (bindVars . #hash((term . ,term) (@tcol .  ,(arango-graphname ar)) )))))
  )


(define (arango-make-edge ar from to reduction [real #t])
  ((arango-send ar)
   "POST"
   "_api/cursor"
   (jsexpr->string
    `#hash(
           (query . "FOR f IN @@tcol  FILTER f.term == @from FOR t IN @@tcol  FILTER t.term == @to UPSERT {\"_from\":f._id,\"_to\":t._id,\"reduction\":@reduction, \"_real\":@real} INSERT {\"_from\":f._id,\"_to\":t._id,\"reduction\":@reduction,\"_real\":@real} UPDATE  OLD IN @@rcol")
           (bindVars . #hash(
                             (from . ,from )
                             (to . ,to)
                             (reduction . ,reduction)
                             (real . ,real )
                             (@tcol . ,(arango-graphname ar) )
                             (@rcol . ,(~a (arango-graphname ar) "-reductions") )
                             ))))
   )
  )


;(define-values (get post clearall lookup fr ed) (createArango "graphredex-data" "terms-test-1"))
;(lookup "Jugs 0 3")
;(lookup "Jugs 0 8")
;(fr #hash((term . "Jugs 0 3")) #t)
;(fr #hash((term . "Jugs 5 3")) #f)
;(ed "Jugs 0 3" "Jugs 5 3" "fill")
