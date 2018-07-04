#lang racket 
(require net/rfc6455)
(require mzlib/string)

(define (echo-handler c state) 
 (define (loop)
  (let* ((received (ws-recv c #:payload-type 'text))
        (term (read-from-string received)))
   (unless (eof-object? received)
    (ws-send! c (format "You said ~v " term)))
  (loop)))
 (loop)
 (display "Socked was closed\n")
 (ws-close! c))

(define stop-service (ws-serve #:port 8081 echo-handler))
(display (read-from-string "'(+ 2 3)"))

(printf "Server running. Hit enter to stop service.\n")

(void (read-line))
(stop-service)


