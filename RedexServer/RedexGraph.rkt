#lang racket
(require net/rfc6455)
(require mzlib/string)
(provide run-server make-graph-with-relation show-dot)
(require redex)
(require graph)
(require json)
(require file/md5)
(require file/sha1)
;  ________                    .__      __________           .___             
; /  _____/___________  ______ |  |__   \______   \ ____   __| _/____ ___  ___
;/   \  __\_  __ \__  \ \____ \|  |  \   |       _// __ \ / __ |/ __ \\  \/  /
;\    \_\  \  | \// __ \|  |_> >   Y  \  |    |   \  ___// /_/ \  ___/ >    < 
; \______  /__|  (____  /   __/|___|  /  |____|_  /\___  >____ |\___  >__/\_ \
;        \/           \/|__|        \/          \/     \/     \/    \/      \/
; Christophe.Scholliers@UGent and Thomas.Dupriez@ens-paris-saclay.fr
;

; Code to make a graph from a reduction relation and a term
; This will only work for finite expansions
(define (make-graph-with-relation relation term) 
  (define g (unweighted-graph/directed '()))
  (define (add-if-new v)
    (when (not (has-vertex? g v))
      (add-vertex! g v)))
  (define (add-edge! g u)
    (lambda (v) (add-directed-edge! g u v))) 
  (define (expand-term term)
    (let ((next-terms (apply-reduction-relation relation term)))
      (add-if-new term)
      (map add-if-new next-terms) 
      (map (add-edge! g term) next-terms)
      (map expand-term next-terms)))
  (expand-term term)
  g)

;; Function to print out the dot information 
(define (show-dot g) (display (graphviz g)))


(define (add-kv k v l)
  (cons (cons k v) l))

(define (jsont t)
  (lambda (e)
   (let* ((term (expr->string e))
          (kv_list (add-kv  'term term (t e)))
          (id  (bytes->hex-string (md5 term))))
     (make-hash (add-kv 'md5 id kv_list)))))

(define (trans->json t ts trans)
 (jsexpr->string 
  (make-hash
   (list 
    (cons 'from (trans t))
    (cons 'next (map trans ts))))))

(define (new-handler relation trans)	
 (define (echo-handler c state) 
  (define (loop)
   (let* ((received   (ws-recv c #:payload-type 'text))
    (term       (read-from-string received))
    (next-terms (apply-reduction-relation relation term))
    (json       (trans->json term next-terms trans)))
    (printf "Receiving ~a\n" received)
    (unless (eof-object? received)
     (printf "Sending ~a\n" json)
     (ws-send! c json))
    (loop)))
  (loop)
  (printf "Web Socked was closed\n")
  (ws-close! c))
 echo-handler)

(define (run-server relation trans)
 (let ((stop-service (ws-serve #:port 8081 (new-handler relation (jsont trans)))))
  (printf "Graph-Redex-Server Running. Hit enter to stop service.\n")
  (void (read-line))
  (stop-service)))
