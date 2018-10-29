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







(define (run-echo relation trans)
 ; clear DB
 (doNeo4j "MATCH (n) OPTIONAL MATCH (n)-[r]-() DELETE n,r")
 ;
 (let* 
    (
      (term (read-from-string (read-line (current-input-port) 'any)))
      (betterTrans (lambda (x) (make-hash (cons (cons 'term (expr->string term)) (trans x)))))
      (next-terms (apply-reduction-relation/tag-with-names relation term))
      (json       (trans->json2 term next-terms (lambda (x) (match x [(list a b) (make-hash (list (cons 'rule a) (cons 'term (expr->string b)) (cons 'data (make-hash (trans b)))))]))))
    )
    (doNeo4jP "CREATE (base:Term {term}) RETURN base"
          `#hash(
            (term . 
              (unquote (betterTrans term))
              )))
    (printf "Out: ~a\n" json)
  )
)