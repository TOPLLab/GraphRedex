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

;
; Wrapper function over the translation function 
; This is to make sure that we always have a term and md5 field
;
(define (make-kv-list exp trans)
 (match exp
  [`(,tag ,e) 
  (add-kv 'rule  tag 
   (list 
    (cons 'term_object
    (make-hash 
     (add-kv  'term (expr->string e) 
;      (add-kv 'md5  (bytes->hex-string (md5 (expr->string e)))
;       (trans e)))))))]))
       (trans e))))))]))

(define (jsont t)
 (lambda (e)
  (make-hash (make-kv-list e t))))

;
; Convert a term into it's json representation 
; by using the translation function 
;
(define (trans->json messageId t ts trans)
 (jsexpr->string 
  (make-hash
   (list 
    (cons 'messageId messageId)
    (cons 'from (trans `("TEST" ,t)))
    (cons 'next (map trans ts))))))

;
; Constants for extracting data from the message 
; A message is a string with a seperator in the middle
; "MESSAGE_ID ##### TERM"
; 
(define SEPERATOR "#####")
(define ID_IDX     0)
(define TRM_IDX    1) 

;
; Extraction functions
;
(define (extract-message-id msg) 
  (list-ref (string-split msg SEPERATOR) ID_IDX))

(define (extract-term msg)
  (read-from-string (list-ref (string-split msg SEPERATOR) TRM_IDX)))

;
; Handler for processing incoming data 
;
(define (new-handler relation trans)	
 (define (echo-handler c state) 
  (define (loop)
   (let* ((received (ws-recv c #:payload-type 'text))
    (messageId  (extract-message-id received))
    (term       (extract-term received))
    (next-terms (apply-reduction-relation/tag-with-names relation term))
    (json       (trans->json messageId term next-terms trans)))
    (printf "Receiving: ~a\n" received)
    (unless (eof-object? received)
     (printf "Sending: ~a\n" json)
     (ws-send! c json))
    (loop)))
  (loop)
  (printf "Web Socked was closed\n")
  (ws-close! c))
 echo-handler)

;
; 
;
(define (run-server relation trans)
 (let ((stop-service (ws-serve #:port 8081 (new-handler relation (jsont trans)))))
  (printf "Graph-Redex-Server Running. Hit enter to stop service.\n")
  (void (read-line))
  (stop-service)))
