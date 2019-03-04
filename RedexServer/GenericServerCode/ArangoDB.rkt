#lang racket

(require net/http-client)
(require json)
(provide createArango)

; http://localhost:8529/_db/graphredex-data/
(define (createArango dbname graphname)
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
                       (unquote (~a "Authorization: bearer " token))
                       )))

    (values
      ; get
      (lambda (x) (sendArango "GET" newHeaders x ""))
      ; post
      (lambda (x y) (sendArango "POST" newHeaders x y))
      (lambda ()
        (sendArango "PUT" newHeaders  (~a "_api/collection/" graphname "-reductions" "/truncate") "")
        (sendArango "PUT" newHeaders  (~a "_api/collection/" graphname "/truncate") "")
        )
      (lambda (query binds [addTcol #f] [addRcol #f])
        (let* (
               (tmpbinds (if addTcol (hash-set binds '@tcol graphname) binds))
               (newbinds (if addRcol (hash-set tmpbinds '@rcol (~a graphname "-reductions")) tmpbinds))
            )
        (sendArango "POST" newHeaders  "_api/cursor"   (jsexpr->string
                      `#hash(
                             (query . (unquote query))
                             (bindVars . (unquote newbinds)))))

        )
      )
      ; Get id of term (if exists)
      (lambda (term)
        (hash-ref (sendArango
                    "POST"
                    newHeaders
                    "_api/cursor"
                    (jsexpr->string
                      `#hash(
                             (query . "FOR doc IN @@tcol FILTER doc.term == @term LIMIT 1 RETURN doc._id")
                             (bindVars . #hash((term . (unquote term )) (@tcol . (unquote graphname))  ))))) 'result))
      ; Create term if not exists and always return id
      ; Marks as expanded if expanded is #t else leaves it
      ; Marks as stuck if stuck is #t else leaves it
      (lambda (term expanded stuck)
        (sendArango
          "POST"
          newHeaders
          "_api/cursor"
          (jsexpr->string
            `#hash(
                   (query .
                          (unquote (if expanded
                                       (~a "LET updatedTerm = MERGE(@term,{\"_expanded\": true, \"_stuck\": " (if stuck "true" "false") "})  UPSERT {term:@term.term} INSERT updatedTerm UPDATE updatedTerm IN @@tcol RETURN NEW._id")
                                       "LET updatedTerm = MERGE(@term,{\"_expanded\": false}) UPSERT {term:@term.term} INSERT updatedTerm UPDATE OLD IN @@tcol RETURN NEW._id")))
                   (bindVars . #hash((term . (unquote term )) (@tcol . (unquote graphname)) ))))))

      (lambda (from to reduction [real #t])
        (sendArango
          "POST"
          newHeaders
          "_api/cursor"
          (jsexpr->string
            `#hash(
                   (query . "FOR f IN @@tcol  FILTER f.term == @from FOR t IN @@tcol  FILTER t.term == @to UPSERT {\"_from\":f._id,\"_to\":t._id,\"reduction\":@reduction, \"_real\":@real} INSERT {\"_from\":f._id,\"_to\":t._id,\"reduction\":@reduction,\"_real\":@real} UPDATE  OLD IN @@rcol")
                   (bindVars . #hash(
                                     (from . (unquote from ))
                                     (to . (unquote to ))
                                     (reduction . (unquote reduction ))
                                     (real . (unquote real ))
                                     (@tcol . (unquote graphname ))
                                     (@rcol . (unquote (~a graphname "-reductions") ))
                                     ))))))
      )
    )
  )



;(define-values (get post clearall lookup fr ed) (createArango "graphredex-data" "terms-test-1"))
;(lookup "Jugs 0 3")
;(lookup "Jugs 0 8")
;(fr #hash((term . "Jugs 0 3")) #t)
;(fr #hash((term . "Jugs 5 3")) #f)
;(ed "Jugs 0 3" "Jugs 5 3" "fill")
