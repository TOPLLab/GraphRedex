#lang racket
(require redex)
(require graph)
(require net/websocket)

(provide make-graph-with-relation show-dot)

;; Code to make a graph from the reductions 
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




(ws-serve
 #:port 8080
 (λ (wsc _)
   (let loop ()
     (define m (ws-recv wsc))
     (printf "~a\n" m)
     (unless (eof-object? m)
       (ws-send! wsc m)
       (loop))))
 #:conn-headers
 (λ (_ hs)
   (define origin
     (header-value (headers-assq* #"Origin" hs)))
   (values
    (list
     (make-header #"Sec-WebSocket-Origin" origin)
     (make-header #"Sec-WebSocket-Location"
                  #"ws://localhost:8080/"))
    #f)))
