#lang racket
;  ________                    .__      __________           .___             
; /  _____/___________  ______ |  |__   \______   \ ____   __| _/____ ___  ___
;/   \  __\_  __ \__  \ \____ \|  |  \   |       _// __ \ / __ |/ __ \\  \/  /
;\    \_\  \  | \// __ \|  |_> >   Y  \  |    |   \  ___// /_/ \  ___/ >    < 
; \______  /__|  (____  /   __/|___|  /  |____|_  /\___  >____ |\___  >__/\_ \
;        \/           \/|__|        \/          \/     \/     \/    \/      \/
;
; Example to show how to use GraphRedex. 
; This is an adapted version of the threads example from the plt-redex example repository
;
(require redex)
(provide reductions term->kv)


;
; The threadslock language.
; Programs p consists of a store which are key-value pairs and zero or more threads
; Each thread is an expression se
; "getlock x n e" tries to get the lock named x before executing e. This succeeds only if x is worth 0. Taking the lock x puts n in variable x.
; "releaselock x n e" tries to release the lock named x before executing e. This succeeds only if x is worth n. Releasing the lock x puts 0 in variable x.
; The "start" marker at the start of each thread is there to ensure that threads execute their outmost instructions first.
; 
(define-language threadslock
  (p (number (store (x v) ...) (threads e ...)))
  (se (start e ))
  (e (getlock x v e) (releaselock x v e) (+ e e) (set v e e) x v)
  (v number)
  (x variable)
  (pc ((store (x v) ...) tc))
  (tc (threads se ... (start ec) se ...))
  (ec hole (+ ec e) (+ number ec)))

(define reductions
  (reduction-relation
   threadslock

   (==> (+ 0 v_4)  v_4 add0 )

   (==> (+ v_3 v_4) (+ ,(- (term v_3) 1) ,(+ 1 (term v_4)) ) add1)

   (-->
    ((store (x_1 v_1) ... (x_i 0    ) (x_2 v_2) ...) (in-hole tc_1 (getlock x_i v_new e_1)))
    ;------------------------------------------------------------------------------- [getlock]
    ((store (x_1 v_1) ... (x_i v_new) (x_2 v_2) ...) (in-hole tc_1 e_1))
        
    getlock)

   (-->
    ((store (x_1 v_1) ... (x_i v_lock) (x_2 v_2) ...) (in-hole tc_1 (releaselock x_i v_lock e_1)))
    ;------------------------------------------------------------------------------- [releaselock]
    ((store (x_1 v_1) ... (x_i 0     ) (x_2 v_2) ...) (in-hole tc_1 e_1))
        
    releaselock)
   with
   [(--> ((store (x_1 v_1) ... ) (in-hole tc_1  a)) ((store (x_1 v_1) ... ) (in-hole tc_1  b)))
    (==> a b)]
   )

  )


(define (tstate x)
  (match x
    [ `(start (getlock ,_ ,_ ,_)) 1]
    [ _ 0])
  )
  

;
; Extraction function to expose certain information of the redex model as attributes for the nodes
;
(define (term->kv exp)
  (match exp
    [`((store ,store ...) (threads ,threads ...))
     (append
      (map (λ (x) `(,(string->symbol (~a "lock " (car x))) . ,(second x))) store)
      `(
        (|waiting threads| . ,(foldl + 0 (map tstate threads)))
        (_formatted . ,(pretty-format exp 30))
        ))]))

(define theone '((store (x 0) (y 0)) 
                 (threads 
                  (start 
                   (getlock x 1 
                            (getlock y 1 
                                     (releaselock y 1 
                                                  (releaselock x 1 1)))))
                  (start 
                   (getlock y 2 
                            (getlock x 2 
                                     (+ (releaselock x 2 1) (releaselock y 2 5))))
                   ))
                 ))

(traces reductions theone)