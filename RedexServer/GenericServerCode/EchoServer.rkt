#lang racket
(require mzlib/string)
(provide run-echo)
(require redex)
(require json)
(require file/md5)
(require "./Neo4j.rkt")



;  ________                    .__      __________           .___             
; /  _____/___________  ______ |  |__   \______   \ ____   __| _/____ ___  ___
;/   \  __\_  __ \__  \ \____ \|  |  \   |       _// __ \ / __ |/ __ \\  \/  /
;\    \_\  \  | \// __ \|  |_> >   Y  \  |    |   \  ___// /_/ \  ___/ >    < 
; \______  /__|  (____  /   __/|___|  /  |____|_  /\___  >____ |\___  >__/\_ \
;        \/           \/|__|        \/          \/     \/     \/    \/      \/
; Christophe.Scholliers@UGent and Thomas.Dupriez@ens-paris-saclay.fr
;





(define (trans->json2 t ts trans)
 (jsexpr->string 
  (make-hash
   (list 
    (cons 'from (expr->string t))
    (cons 'next (map trans ts))))))

(define (trans->hash t ts trans)
 (make-hash
   (list 
    (cons 'from (trans `("TEST" ,t)))
    (cons 'next (map trans ts)))))



(define (neo4j-node-done term)
(let ((res (doNeo4jP 
    "MATCH (e) WHERE e.term = {term} AND e._expanded = TRUE RETURN ID(e)" 
    `#hash((term . (unquote (expr->string term))))
  )))
  (> (length (hash-ref (car (hash-ref res 'results)) 'data)) 0)
)

)

(define (run-echo relation trans)
  ; clear DB
  (doNeo4j "MATCH (n) OPTIONAL MATCH (n)-[r]-() DELETE n,r")
  (doNeo4j "CREATE CONSTRAINT ON (n:Term) ASSERT n.term IS UNIQUE")
  (let* 
    (
      (term (read-from-string (read-line (current-input-port) 'any)))
    )
    (run-echo2 (list term) 1000 relation trans)
    (doNeo4jP "MATCH (base:Term {term:{term}}) SET base._base=TRUE"
      `#hash((term . (unquote (expr->string term)))))
  )

)


(define (run-echo2 terms cnt relation trans)
 ;
  (printf "\n\n\n------------------------------\n~a\n=======\n" terms)
 (cond
  [(= 0 (length terms)) (printf "\nDONE (~a reductions left)\n" cnt)] 
  [(neo4j-node-done (car terms)) 
    (printf "\nSKIPPED\n")
    (run-echo2 (cdr terms) cnt relation trans)
  ]
  [(positive? cnt) 
  (let* 
    (
      (term (car terms))
      (betterTrans (lambda (x) (make-hash (cons (cons 'term (expr->string x)) (trans x)))))
      (next-terms (apply-reduction-relation/tag-with-names relation term))
      (json       (trans->json2 term next-terms (lambda (x) (match x [(list a b) (make-hash (list (cons 'rule a) (cons 'term (expr->string b)) (cons 'data (make-hash (trans b)))))]))))
    )
    (doNeo4jP "MERGE (base:Term {term:{term}})"
          `#hash( (term . (unquote (expr->string term))           )))

    (doNeo4jP "MATCH (base:Term {term:{term}.term}) SET base={term},base._expanded=TRUE"
          `#hash(
            (term . (unquote (betterTrans term))     )
            ))


    (for ([x next-terms]) (match x [(list rel term2) 
          (printf "\nAdded: ~a -[reduces:~a]-> ~a\n" term rel term2)
          (doNeo4jP "MERGE (base:Term {term:{term}.term})"
                      `#hash((term . (unquote (betterTrans term2)))))
          (doNeo4jP (~a "MATCH  (base:Term {term:{src}}) MATCH (two:Term {term:{dst}}) MERGE (base)-[reduces:`" rel "`]->(two)")
                      `#hash(
                        (src . (unquote (expr->string term)))
                        (dst . (unquote (expr->string term2)))
                      ))
    ]))
    (printf "Added: ~a\n" json)
    (run-echo2 (append (cdr terms) (map (lambda (x) (car (cdr x))) next-terms)) (- cnt 1) relation trans)
  )]
 ) 

)

