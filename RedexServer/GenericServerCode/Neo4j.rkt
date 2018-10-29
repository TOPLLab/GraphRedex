#lang racket

(require net/http-client)
(require json)
(provide doNeo4j doNeo4jP)


(define (sendNeo4j qry)
  (define-values (status headers in)
  (http-sendrecv "localhost"
                 "/db/data/transaction/commit/"
                 #:ssl? #f
                 #:version "1.1"
                 #:method "POST"
                 #:port 7474
                 #:headers  '("Authorization: Basic bmVvNGo6bmVvNGotanMtcGFzc3dvcmQ="
                              "Content-Type: application/json"
                              "Accept: application/json; charset=UTF-8"
                              )
                 #:data qry))
(displayln qry)
;(displayln status)
;(displayln headers)
(let ((result (string->jsexpr (port->string in)))) 
(displayln result)
(close-input-port in)
result
)
)


(define (doNeo4j statement)
  ; Make query as {"statements":[{"statement":"..."}]}
  (let (
    (qry (jsexpr->string `#hash((statements . (#hash((statement . (unquote statement)))))))))
    (sendNeo4j qry)
   )
)

(define (doNeo4jP statement data)
  ; Make query as {"statements":[{"statement":"...", parameters:..}]}
  (let (
    (qry (jsexpr->string `#hash((statements . (#hash(
      (statement . (unquote statement))
      (parameters . (unquote data))
      )))))))
    (sendNeo4j qry)
   )
)


;  EXamples
; 
; (doNeo4j "MATCH (n) OPTIONAL MATCH (n)-[r]-() DELETE n,r")
; 
; 
; (doNeo4jP 
;   "MATCH (e) WHERE e.term = {lol} RETURN ID(e)" 
;   '#hash((lol . "b"))
; )