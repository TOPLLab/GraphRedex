#lang racket
(require redex)

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; all variables have to be initialized in the inits blocks, but uninitialized is a value
;; '=' is only for overwriting
;; Mutexes can only contain simple values, no mutex guards, mutex references or join handles
;; When a mutex guard is created it should be assigned to a variable
;; Overwriting a mutex guard is impossible, the value is assigned to the mutex
;; A function that returns a value should be called as an expression, only a function that doesn't return anything can be called as a statement
(define-language fjm
  (prog (funcs bk))
  (inits ((x : type = val) ...))
  (function (func func-name arg-definition : type bk))
  (funcs (function ...))
  (arg-definition ((x : type) ...))
  (bk (block inits (stmt ...)))
  (stmt (x = expr)
        (return expr)
        join-stmt
        bk
        func-call
        if-stmt)
  (stmts (stmt ...))
  (expr x
     val ;used to be simple-val, may need to reconsider in case of errors
     (expr operator expr)
     fork-expr
     lock-expr
     mutex-init
     func-call)
  (operator + == >)
  (if-stmt (if expr bk))
  (val direct-val)
  (simple-val number ; The values a programmer can enter. The other types of values will be introduced in fjm-machine (next language).
              boolean
              (uninitialized))
  (direct-val simple-val) ; assignment to a variable containing a direct value results in overwriting the value
  (mutex-init (mutex simple-val))
  (func-call (call func-name args))
  (args (expr ...))
  (lock-expr (lock expr))
  (join-stmt (join expr))
  (fork-args ((x : type = expr) ...)) ; should only be copy values
  (fork-expr (fork fork-args bk)) ; The new thread will get the passed values, these should only be copy values
  ((x y func-name) variable)
  (id number)
  (type int
        bool
        (mutex type)
        (mutex-guard type)
        join-handle
        void))


(define-extended-language fjm-machine fjm
  
  ;; To start a program, a prog is converted to a prog-context
  ;; by putting the variables and mutexes from inits in the store and mutexes
  ;; and putting the main block in threads with index 0.
  (prog-context (funcs mutexes thread-context))
  (store ((x val) ...))
  (cleaned-store ((x direct-val) ...)) ; Contains no mutex-guards, so no unlocking has to be done
  (mutexes ((id mutex-state simple-val) ...))
  (val ::= ....
       mutex-guard-val)
  (direct-val ::= ....
              mutex-ref-val
              join-handle-val)
  (mutex-ref-val (mutex-ref id))
  (join-handle-val (join-handle id)) ;; A reference to a thread
  (mutex-guard-val (mutex-guard id)) ;; A reference to a mutex
  (copy-val direct-val) ; assignment of a copy value results in simply assigning the value to the new variable
  (move-val mutex-guard-val) ; assignment of a move value results in both assigning the value to the new variable and assigning uninitialized to the old one.

  ;; A finished thread has the block (block () (()...))
  (thread-context ((id running-block) ... (id bk-context) (id running-block) ...))
  (running-block (block store (finished-stmt ... stmt ...))
                 (block store (finished-stmt ... running-block stmt ...)))
  (running-func-block (func running-block))
  (finished-block (block cleaned-store finished-stmtseq))
  (finished-void-block (block cleaned-store (finished-void-stmt ...))) ; Finished block that doesn't return anything.
  (finished-void-stmt ()
                      finished-void-block)
  (finished-stmt finished-block
                 ())

  ; sequence of statements that won't be evaluated any further on its own
  ; only value can be returned, returning var will first be reduced to returning val
  (finished-stmtseq (finished-stmt ... (return val) stmt ...)
                  (finished-stmt ...))
  ((bk-context scope-context) (block store stmt-context) ; TODO delete 'scope-context'?
                              hole)
  (func-context (func bk-context))
  (stmt-context func-local-stmt-context
                (finished-stmt ... (in-hole func-local-single-stmt-context func-context) stmt ...))
  (func-local-stmt-context (finished-stmt ... func-local-single-stmt-context stmt ...))
  (func-local-single-stmt-context arg-context
                                  fork-context
                                  (x = expr-context)
                                  (join expr-context)
                                  if-context
                                  (return expr-context)
                                  bk-context
                                  hole)
  (if-context (if expr-context bk))
  (arg-context direct-arg-context
               indirect-arg-context)
  (indirect-arg-context (call func-name (expr ... indirect-expr-context val ...)))
  (direct-arg-context (call func-name (expr ... hole val ...)))
  (fork-context direct-fork-context
                indirect-fork-context)
  (indirect-fork-context (fork ((x_1 : type_1 = expr) ... (x_2 : type_2 = indirect-expr-context) (x : type = val) ... ) bk))
  (direct-fork-context (fork ((x_1 : type_1 = expr) ... (x_2 : type_2 = hole) (x : type = val) ... ) bk))
  (finished-args (val ...))
  (expr-context indirect-expr-context
                hole)

  ;; No direct expressions, helps to avoid resolving move variables without moving them
  (indirect-expr-context (expr-context operator expr)
                         (val operator expr-context)
                         (lock expr-context)
                         indirect-arg-context
                         indirect-fork-context)
  (mutex-guard-consumer-context (join expr-context) ; A mutex guard in a hole in this context will be resolved to its value.
                                if-context
                                (x = indirect-expr-context))
  (mutex-state not-locked locked)
  ((new-id mutex-id block-id) number))


;; Recursively removes the type annotations
(define-metafunction fjm-machine
  setup-running-block : bk -> running-block
  ((setup-running-block (block ((x : type = val) ...) (stmt ...))) (block ((x val) ...) (stmt ...))))


;; Makes a store containing the given arguments for executing the given function
(define-metafunction fjm-machine
  args2store : arg-definition finished-args -> store
  ((args2store () ()) ())
  ((args2store ((x_0 : type_0) ... (x : type)) (val_0 ... val))
   (any ... (x val))
   (where (any ...) (args2store ((x_0 : type_0) ...) (val_0 ...)))))

;; Makes a running func block for a function call
(define-metafunction fjm-machine
  setup-running-func-block : funcs func-name finished-args -> running-func-block
  ((setup-running-func-block
    (function_1 ... (func func-name arg-definition : type bk) function_2 ...)
    func-name
    (val_0 ... val))
   (func (block (args2store arg-definition (val_0 ... val)) ((setup-running-block bk))))))


(define-metafunction fjm-machine
  get-value : prog-context x -> val
  ((get-value (in-hole prog-context (block (any_1 ... (x val) any_2 ...) func-local-stmt-context)) x)
   val
   (side-condition (not (redex-match?
                         fjm-machine
                         ((in-hole func-local-stmt-context_outer (block (any_3 ... (x val_other) any_4 ...) func-local-stmt-context_inner)) x)
                         (term (func-local-stmt-context x)))))))


(define-metafunction fjm-machine
  assign-value : prog-context x val -> prog-context
  ((assign-value (in-hole prog-context (block (any_1 ... (x val_old) any_2 ...) func-local-stmt-context)) x val)
   (in-hole prog-context (block (any_1 ... (x val) any_2 ...) func-local-stmt-context))
   (side-condition (not (redex-match?
                         fjm-machine
                         ((in-hole func-local-stmt-context_outer (block (any_3 ... (x val_other) any_4 ...) func-local-stmt-context_inner)) x)
                         (term (func-local-stmt-context x)))))))


(define-metafunction fjm-machine
  new-mutex-id : mutexes -> number
  ((new-mutex-id ()) 0)
  ((new-mutex-id ((id_1 mutex-state_1 val_1) ... (id_i mutex-state_i val_i))) ,(+ (term id_i) 1)))


(define-metafunction fjm-machine
  count-threads : thread-context -> number
  ((count-threads ((id_1 any_1) ... (id_i any_i))) ,(+ (term id_i) 1)))


(define machine-step
  (reduction-relation
   fjm-machine

   ;; Set up a machine for executing the program
   (--> ((function ...) bk)
        ((function ...) () ((0 (setup-running-block bk))))
        setup)

   ;; Convert the inits of a block to a store
   (--> (in-hole prog-context bk)
        (in-hole prog-context (setup-running-block bk))
        (where (block ((x_0 : type_0 = val_0)(x : type = val) ...) (stmt ...)) bk)
        block)

   (--> (in-hole prog-context (number_1 + number_2))
        (in-hole prog-context ,(+ (term number_1) (term number_2)))
        sum)

   (--> (in-hole prog-context (val_1 == val_2))
        (in-hole prog-context ,(equal? (term val_1) (term val_2)))
        equals)

   (--> (in-hole prog-context (val_1 > val_2))
        (in-hole prog-context ,(> (term val_1) (term val_2)))
        greater-than)

   (--> (in-hole prog-context (if #true bk))
        (in-hole prog-context bk)
        if-true)

   (--> (in-hole prog-context (if #false bk))
        (in-hole prog-context ())
        if-false)

   (--> (in-hole prog-context (call func-name finished-args))
        (in-hole prog-context (setup-running-func-block funcs func-name finished-args))
        (where (funcs mutexes thread-context) prog-context)
        function-call)

   (--> (in-hole prog-context (x_1 = x_2))
        (in-hole prog-context_new ())
        (where copy-val (get-value prog-context x_2))
        (where prog-context_new (assign-value prog-context x_1 copy-val))
        (where direct-val (get-value prog-context x_1)) ; so it's not a mutex guard
        copy)

   (--> (in-hole prog-context (x_1 = x_2))
        (in-hole prog-context_new ())
        (where move-val (get-value prog-context x_2))
        (where prog-context_new (assign-value (assign-value prog-context x_1 move-val) x_2 (uninitialized)))
        move)

   (--> (in-hole prog-context (in-hole direct-arg-context x))
        (in-hole prog-context (in-hole direct-arg-context copy-val))
        (where copy-val (get-value prog-context x))
        copy-arg)

   (--> (in-hole prog-context (in-hole direct-arg-context x))
        (in-hole prog-context_new (in-hole direct-arg-context move-val))
        (where move-val (get-value prog-context x))
        (where prog-context_new (assign-value prog-context x (uninitialized)))
        move-arg)

   ;; other fork arguments are treated by the other rules, they are holes in a prog-context
   (--> (in-hole prog-context (in-hole direct-fork-context x))
        (in-hole prog-context (in-hole direct-fork-context copy-val))
        (where copy-val (get-value prog-context x))
        fork-arg)

   (--> (in-hole prog-context (in-hole func-context (return val)))
        (in-hole prog-context val)
        (where (func finished-block) (in-hole func-context (return val)))
        return-val)

   (--> (in-hole prog-context (return x))
        (in-hole prog-context_new (return val))
        (where val (get-value prog-context x))
        (where prog-context_new (assign-value prog-context x (uninitialized))) ; needed in case of a move value, not problematic otherwise
        return-local-var)

   (--> (in-hole prog-context (func finished-void-block))
        (in-hole prog-context ())
        return-void)

   (--> (in-hole prog-context x)
        (in-hole prog-context val)
        (where (in-hole prog-context_0 indirect-expr-context) prog-context)
        (where val (get-value prog-context x))
        resolve-name)

   (--> (in-hole prog-context x)
        (in-hole prog-context val)
        (where (in-hole prog-context_0 if-context) prog-context)
        (where val (get-value prog-context x))
        if-resolve-name)

   (--> (in-hole prog-context (mutex-guard id))
        (in-hole prog-context val)
        (where (funcs mutexes thread-context) prog-context)
        (where (any_1 ... (id locked val) any_2 ...) mutexes)
        (where (in-hole thread-context_0 mutex-guard-consumer-context) thread-context)
        resolve-mutex-guard)

   (--> (in-hole prog-context (x = val))
        (in-hole prog-context_new ())
        (where direct-val (get-value prog-context x)) ; so it's not a mutex guard
        (where prog-context_new (assign-value prog-context x val))
        assign)

   ; The location of x_i, which is ensured by (get-value prog-context x_i), ensures that the current
   ; thread holds the lock.
   (--> (in-hole prog-context (x_i = val))
        (in-hole prog-context_new ())
        (where (funcs mutexes thread-context) prog-context)
        (where (any_mutex1 ... (mutex-id locked val_old) any_mutex2 ...) mutexes)
        (where (mutex-guard mutex-id) (get-value prog-context x_i))
        (where prog-context_new (funcs (any_mutex1 ... (mutex-id locked val) any_mutex2 ...) thread-context))
        mutex-guard-val-overwrite)

   (--> (in-hole prog-context (x_i = x_j))
        (in-hole prog-context_new ())
        (where (funcs mutexes thread-context) prog-context)
        (where (any_mutex1 ... (mutex-id locked val_old) any_mutex2 ...) mutexes)
        (where (mutex-guard mutex-id) (get-value prog-context x_i))
        (where val (get-value prog-context x_j))
        (where prog-context_new (funcs (any_mutex1 ... (mutex-id locked val) any_mutex2 ...) thread-context))
        (where direct-val (get-value prog-context x_j)) ; so it's not a mutex guard
        mutex-guard-var-overwrite)

   (--> (in-hole prog-context (x_i = x_j))
        (in-hole prog-context_new ())
        (where (funcs mutexes thread-context) prog-context)
        (where (any_mutex1 ... (mutex-id locked val_old) any_mutex2 ...) mutexes)
        (where (mutex-guard mutex-id) (get-value prog-context x_i))
        (where (mutex-guard id_val) (get-value prog-context x_j))
        (where (funcs mutexes thread-context) prog-context)
        (where (any_1 ... (id_val locked val) any_2 ...) mutexes)
        (where prog-context_new (funcs (any_mutex1 ... (mutex-id locked val) any_mutex2 ...) thread-context))
        mutex-guard-mutex-guard-overwrite)

   (--> (funcs ((id_1 mutex-state_1 val_1) ...)
         (in-hole thread-context (mutex val)))
        (funcs ((id_1 mutex-state_1 val_1) ... (new-id not-locked val))
         (in-hole thread-context (mutex-ref new-id)))
         (where new-id (new-mutex-id ((id_1 mutex-state_1 val_1) ...)))
        new-mutex)

   (--> (funcs mutexes
         (in-hole thread-context (fork inits bk)))
        (funcs mutexes
         (in-hole (any ... (new-id (setup-running-block (block inits (bk))))) (join-handle new-id)))
        (where (any ...) thread-context )
        (where new-id (count-threads thread-context))
        fork)

   (--> (in-hole prog-context (join x))
        (in-hole prog-context ())
        (where (join-handle id) (get-value prog-context x))
        (where (funcs mutexes (any_1 ... (id finished-block) any_2 ...)) prog-context)
        join)

   (--> (funcs (any_mutex ... (mutex-id not-locked val) any_mutex2 ...)
         (in-hole thread-context (lock (mutex-ref mutex-id))))
        (funcs (any_mutex ... (mutex-id locked val) any_mutex2 ...)
         (in-hole thread-context (mutex-guard mutex-id)))
        lock)

   (--> (funcs (any_mutex ... (mutex-id locked val) any_mutex2 ...)
         (in-hole thread-context (block (any_var ... (x (mutex-guard mutex-id)) (x_1 direct-val) ...)
                                        finished-stmtseq)))
        (funcs (any_mutex ... (mutex-id not-locked val) any_mutex2 ...)
         (in-hole thread-context (block (any_var ... (x_1 direct-val) ...)
                                        finished-stmtseq)))
         unlock)
         
   
   ))



;
; Extraction function to expose certain information of the redex model as attributes for the nodes
;
(define (term->kv exp)
  (list (cons 'x 5) ))
 

(require "./GenericServerCode/EchoServer.rkt")
(run-echo machine-step term->kv)
