#lang typed/racket

(require "../types/types.rkt"
         "../Parse/parse.rkt"
         "../Reference/ref-sig.rkt"
         "../Reference/ref-unit.rkt"
         "../Continuation/cont-sig.rkt"
         "../Continuation/cont-unit.rkt"
         "../ExpValues/values-sig.rkt"
         "../ExpValues/values-unit.rkt"
         "../Procedure/proc-sig.rkt"
         "../Procedure/proc-unit.rkt"
         "../Environment/env-sig.rkt"
         "../Environment/env-unit.rkt"
         "../Expressions/exp-sig.rkt"
         "../Expressions/exp-unit.rkt")

(require "../Modules/thread.rkt"
         "../Modules/exit.rkt")

(provide (all-from-out "../types/types.rkt")
         (all-from-out "../Parse/parse.rkt")
         (all-from-out "../Modules/thread.rkt")
         (all-from-out "../Modules/exit.rkt")
         (all-defined-out))


(define-compound-unit/infer base@
  (import)
  (export ref^ cont^ values^ env^ proc^ exp^)
  (link   ref@ cont@ values@ env@ proc@ exp@))

(define-values/invoke-unit base@
  (import)
  (export ref^ cont^ values^ env^ proc^ exp^))


(define-namespace-anchor ns-anchor)
(define eval-ns (namespace-anchor->namespace ns-anchor))

(: *eval* [->* (S-Exp Env Cont) (Exact-Positive-Integer) ExpVal])
(define *eval*
  (λ (code env cont [timeslice 1])
    (: exp Exp)
    (define exp
      (assert (call-with-values
               (λ ()
                 (eval
                  (parser
                   (desugar
                    (auto-cps
                     (desugar
                      (module/exit
                       (module/thread
                        (auto-apply
                         (desugar
                          code))
                        timeslice))))))
                  eval-ns))
               (λ args (car args)))
              exp?))


    #;(pretty-print code)
    #;(pretty-print exp)
    (value-of/k exp env cont)))

(let ()
  (: +eval+ [-> S-Exp Env ExpVal])
  (define +eval+
    (λ (code env)
      (: exp Exp)
      (define exp
        (assert (call-with-values
                 (λ ()
                   (eval
                    #;(parse code)
                    (parser
                     (desugar
                      (auto-cps
                       (desugar
                        ;; To define `mul-thread`, we can't use `auto-apply' pass.
                        code))))
                    eval-ns))
                 (λ args (car args)))
                exp?))

      #;(pretty-print code)
      #;(pretty-print exp)
      (value-of/k exp env (id-cont))))

  (: ~eval~ [-> S-Exp Env ExpVal])
  (define ~eval~
    (λ (code env)
      (: exp Exp)
      (define exp
        (assert (call-with-values
                 (λ ()
                   (eval
                    (parser
                     (desugar
                      (auto-cps
                       (desugar
                        ;; To define `mul-thread`, we can't use `auto-apply' pass.
                        code))))
                    eval-ns))
                 (λ args (car args)))
                exp?))

      #;(pretty-print code)
      #;(pretty-print exp)
      (value-of/k exp env (id-cont))))

  (: nullary-func [-> Symbol [-> Any] [-> DenVal * ExpVal]])
  (define nullary-func
    (λ (name func)
      (λ vals
        (match vals
          ['() (s-expval->expval (func))]
          [_ (error name "Bad args: ~s" vals)]))))


  (: unary-pred [-> Symbol [-> Any Boolean] [-> DenVal * ExpVal]])
  (define unary-pred
    (λ (name pred)
      (λ vals
        (match vals
          [`(,val) (bool-val (pred (expval->denval val)))]
          [_ (error name "Bad args: ~s" vals)]))))

  (: unary-length (All (A) [-> Symbol (pred A) [-> A Index] [-> DenVal * ExpVal]]))
  (define unary-length
    (λ (name pred len)
      (λ vals
        (match vals
          [`(,val) #:when (pred val) (num-val (len val))]
          [_ (error name "Bad args: ~s" vals)]))))

  (: unary-destruct (All (A) [-> Symbol (pred A) [-> A DenVal] [-> DenVal * ExpVal]]))
  (define unary-destruct
    (λ (name pred destruct)
      (λ vals
        (match vals
          [`(,val) #:when (pred val) (destruct val)]
          [_ (error name "Bad args: ~s" vals)]))))

  (: binary-ref (All (A B) [-> Symbol (pred A) (pred B) [-> A B DenVal] [-> DenVal * ExpVal]]))
  (define binary-ref
    (λ (name pred key? ref)
      (λ vals
        (match vals
          [`(,val-1 ,val-2)
           #:when (and (pred val-1) (key? val-2))
           (ref val-1 val-2)]
          [_ (error name "Bad args: ~s" vals)]))))

  (: unary-arithmetic-pred [-> Symbol [-> Real Boolean] [-> DenVal * ExpVal]])
  (define unary-arithmetic-pred
    (λ (name pred)
      (λ vals
        (match vals
          [`(,val) (bool-val (pred (expval->num val)))]
          [_ (error name "Bad args: ~s" vals)]))))

  (: unary-arithmetic-func [-> Symbol [-> Real Real] [-> DenVal * ExpVal]])
  (define unary-arithmetic-func
    (λ (name func)
      (λ vals
        (match vals
          [`(,val) (num-val (func (expval->num val)))]
          [_ (error name "Bad args: ~s" vals)]))))

  (: unary-func [-> Symbol [-> Any Any] [-> DenVal * ExpVal]])
  (define unary-func
    (λ (name func)
      (λ vals
        (match vals
          [`(,val) (s-expval->expval (func val))]
          [_ (error name "Bad args: ~s" vals)]))))


  (: binary-imhash [-> Symbol
                       [->* () #:rest-star (DenVal DenVal) (Immutable-HashTable DenVal DenVal)]
                       [-> DenVal * ExpVal]])
  (define binary-imhash
    (λ (name make)
      (λ vals
        (match vals
          ['()              (make)]
          [`(,val-1 ,val-2) (make val-1 val-2)]
          [_ (error name "Bad args: ~s" vals)]))))

  (: unary-mhash [-> Symbol
                     [->* () ((Listof (Pairof DenVal DenVal))) (Mutable-HashTable DenVal DenVal)]
                     [-> DenVal * ExpVal]])
  (define unary-mhash
    (λ (name make)
      (λ vals
        (match vals
          ['()     (make)]
          [`(,val) #:when ((listof? denpair?) val) (make val)]
          [_ (error name "Bad args: ~s" vals)]))))

  (: binary-equal-relation [-> Symbol [-> Any Any Boolean] [-> DenVal * ExpVal]])
  (define binary-equal-relation
    (λ (name relation)
      (λ vals
        (match vals
          [`(,val-1 ,val-2)
           (bool-val (relation val-1 val-2))]
          [_ (error name "Bad args: ~s" vals)]))))


  (: binary-arithmetic-relation [-> Symbol [-> Real Real Boolean] [-> DenVal * ExpVal]])
  (define binary-arithmetic-relation
    (λ (name relation)
      (λ vals
        (match vals
          [`(,val-1 ,val-2)
           (bool-val (relation (expval->num val-1)
                               (expval->num val-2)))]
          [_ (error name "Bad args: ~s" vals)]))))


  (: n-ary-arithmetic-func [-> Symbol [-> Real Real * Real] [-> DenVal * ExpVal]])
  (define n-ary-arithmetic-func
    (λ (name func)
      (λ vals
        (match vals
          [`(,val-1 . ,(? list? vals))
           (num-val (apply func
                           (expval->num val-1)
                           (map (λ ([val : DenVal]) : Real
                                  (expval->num val))
                                vals)))]
          [_ (error name "Bad args: ~s" vals)]))))

  (: n-ary-logic-func [-> [-> Boolean * Boolean] [-> DenVal * ExpVal]])
  (define n-ary-logic-func
    (λ (func)
      (λ vals
        (bool-val (apply func
                         (map (λ ([val : DenVal]) : Boolean
                                (expval->bool val))
                              vals))))))


  (: add-primitive-proc! [-> Symbol [-> DenVal * ExpVal] Void])
  (define add-primitive-proc!
    (λ (op-name op-val)
      (define k  (gensym 'k))
      (define vs (gensym 'vs))

      (add-denval! op-name
                   (procedure
                    (list k)
                    (proc-exp vs
                              (call-exp (var-exp k)
                                        (list (call-exp (var-exp op-name)
                                                        (var-exp vs)))))
                    (extend-env op-name (primitive-proc op-val) (empty-env))))))

  (: add-denval! [-> Symbol DenVal Void])
  (define add-denval!
    (λ (name val)
      #;(displayln name)
      (if (has-binding? (base-env) name)
          (set-binding! (base-env) name val)
          (base-env (extend-env name val (base-env))))))


  (add-denval! 'undefined undefined)
  (add-denval! 'null      null)
  (add-denval! 'empty     empty)
  (add-denval! 'true      true)
  (add-denval! 'false     false)


  (add-primitive-proc! 'exit
                       (λ [vals : DenVal *] : ExpVal
                         (match vals
                           [`(,val) (exit val)]
                           [_ (error 'exit "Bad args: ~s" vals)])))
  (add-primitive-proc! 'raise
                       (λ [vals : DenVal *] : ExpVal
                         (match vals
                           [`(,val) (error "uncaught exception:" (expval->denval val))]
                           [_ (error 'raise "Bad args: ~s" vals)])))
  (add-primitive-proc! 'eval
                       (λ [vals : DenVal *] : ExpVal
                         (match vals
                           [`(,code)
                            #:when (s-exp? code)
                            (*eval* code (base-env) (id-cont))]
                           [_ (error 'eval "Bad args: ~s" vals)])))
  (add-denval! 'apply
               (proc-val
                (procedure '(k)
                           (proc-exp '(func args)
                                     (call-exp (call-exp (var-exp 'func)
                                                         (list (var-exp 'k)))
                                               (var-exp 'args)))
                           (empty-env))))
  #;(add-primitive-proc! 'apply
                         ;; k should be passed to func, so this code won't work.
                         (λ [vals : DenVal *] : ExpVal
                           (match vals
                             [`(,func ,args)
                              #:when (and (proc? func)
                                          ((listof? denval?) args))
                              (apply-procedure/k func args (id-cont))]
                             [_ (error 'apply "Bad args: ~s" vals)])))


  (add-primitive-proc! 'identity (unary-func  'identity identity))

  (add-primitive-proc! 'empty-queue (nullary-func 'empty-queue empty-queue))
  (add-primitive-proc! 'empty-list  (λ [vals : DenVal *] : ExpVal '()))

  (add-primitive-proc! 'boolean?   (unary-pred 'boolean?   boolean?))
  (add-primitive-proc! 'real?      (unary-pred 'real?      real?))
  (add-primitive-proc! 'char?      (unary-pred 'char?      char?))
  (add-primitive-proc! 'string?    (unary-pred 'string?    string?))
  (add-primitive-proc! 'bytes?     (unary-pred 'bytes?     bytes?))
  (add-primitive-proc! 'symbol?    (unary-pred 'symbol?    symbol?))
  (add-primitive-proc! 'undefined? (unary-pred 'undefined? undefined?))
  (add-primitive-proc! 'void?      (unary-pred 'void?      void?))

  (add-primitive-proc! 'not    (unary-pred 'not    not))
  (add-primitive-proc! 'false? (unary-pred 'false? false?))
  (add-primitive-proc! 'true?  (unary-pred 'true?  true?))

  (add-primitive-proc! 'null?        (unary-pred 'null?        null?))
  (add-primitive-proc! 'list?        (unary-pred 'list?        list?))
  (add-primitive-proc! 'empty-queue? (unary-pred 'empty-queue? empty-queue?))
  (add-primitive-proc! 'queue?       (unary-pred 'queue?       queue?))
  (add-primitive-proc! 'immutable?   (unary-pred 'immutable?   immutable?))
  (add-primitive-proc! 'box?         (unary-pred 'box?         box?))
  (add-primitive-proc! 'pair?        (unary-pred 'pair?        pair?))
  (add-primitive-proc! 'vector?      (unary-pred 'vector?      vector?))
  (add-primitive-proc! 'hash?        (unary-pred 'hash?        hash?))

  (add-primitive-proc! 'exact-positive-integer? (unary-pred 'exact-positive-integer? exact-positive-integer?))
  (add-primitive-proc! 'zero? (unary-arithmetic-pred 'zero? zero?))
  (add-primitive-proc! 'sub1  (unary-arithmetic-func 'sub1  sub1))
  (add-primitive-proc! 'add1  (unary-arithmetic-func 'add1  add1))
  (add-primitive-proc! '-1+   (unary-arithmetic-func '-1+   -1+))
  (add-primitive-proc! '1+    (unary-arithmetic-func '1+    1+))
  (add-primitive-proc! '1-    (unary-arithmetic-func '1-    1-))


  (add-primitive-proc! 'reverse
                       (λ [vals : DenVal *] : ExpVal
                         (match vals
                           [`(,val) #:when ((listof? denval?) val) (reverse val)]
                           [_ (error 'reverse "Bad args: ~s" vals)])))


  (add-primitive-proc! 'string-length (unary-length 'string-length string?           string-length))
  (add-primitive-proc! 'bytes-length  (unary-length 'bytes-length  bytes?            bytes-length))
  (add-primitive-proc! 'length        (unary-length 'length        (listof? denval?) (inst length DenVal)))
  (add-primitive-proc! 'vector-length (unary-length 'vector-length denvector?        vector-length))

  (add-primitive-proc! 'string-ref (binary-ref 'string-ref string?           index? string-ref))
  (add-primitive-proc! 'bytes-ref  (binary-ref 'bytes-ref  bytes?            index? bytes-ref))
  (add-primitive-proc! 'list-ref   (binary-ref 'list-ref   (listof? denval?) index? (inst list-ref   DenVal)))
  (add-primitive-proc! 'vector-ref (binary-ref 'vector-ref denvector?        index? (inst vector-ref DenVal)))
  (add-primitive-proc! 'hash-ref
                       (λ vals
                         (match vals
                           [`(,val-1 ,val-2)
                            #:when (denhash? val-1)
                            (hash-ref val-1 val-2)]
                           [`(,val-1 ,val-2 ,val-3) ; `val-3' is failure-result
                            #:when (and (denhash? val-1)
                                        (false? val-3))
                            (hash-ref val-1 val-2 val-3)]
                           [_ (error 'hash-ref "Bad args: ~s" vals)])))

  (add-primitive-proc! 'unbox (unary-destruct 'unbox denbox?  (inst unbox DenVal)))
  (add-primitive-proc! 'car   (unary-destruct 'car   denpair? (inst car DenVal DenVal)))
  (add-primitive-proc! 'cdr   (unary-destruct 'cdr   denpair? (inst cdr DenVal DenVal)))
  (let ()
    (: get-op [-> String Symbol])
    (define get-op (λ (ad*) (string->symbol (string-append "c" ad* "r"))))

    (void
     (for/fold ([prevs : (Listof String) '("a" "d")])
               ([i (in-range 1 4)])
       (for*/list : (Listof String)
                  ([curr (in-list '("a" "d"))]
                   [prev (in-list prevs)])
         (define now (string-append curr prev))
         (add-denval! (get-op now)
                      (expval->denval
                       (+eval+ `(λ (arg)
                                  (,(get-op curr)
                                   (,(get-op prev)
                                    arg)))
                               (base-env))))
         now))))


  (add-primitive-proc! 'read    (nullary-func 'read    read))
  (add-primitive-proc! 'newline (nullary-func 'newline newline))

  (add-primitive-proc! 'display (unary-func 'display display))
  (add-primitive-proc! 'print   (unary-func 'print   print))
  (add-primitive-proc! 'write   (unary-func 'write   write))

  (add-primitive-proc! 'displayln (unary-func 'displayln displayln))
  (add-primitive-proc! 'println   (unary-func 'println   println))
  (add-primitive-proc! 'writeln   (unary-func 'writeln   writeln))

  (add-primitive-proc! 'pretty-display (unary-func 'pretty-display pretty-display))
  (add-primitive-proc! 'pretty-print   (unary-func 'pretty-print   pretty-print))
  (add-primitive-proc! 'pretty-write   (unary-func 'pretty-write   pretty-write))


  (add-primitive-proc! '=  (binary-arithmetic-relation '=  =))
  (add-primitive-proc! '>  (binary-arithmetic-relation '>  >))
  (add-primitive-proc! '>= (binary-arithmetic-relation '>= >=))
  (add-primitive-proc! '<  (binary-arithmetic-relation '<  <))
  (add-primitive-proc! '<= (binary-arithmetic-relation '<= <=))


  (add-primitive-proc! 'cons
                       (λ [vals : DenVal *] : ExpVal
                         (match vals
                           [`(,val-1 ,val-2) (pair-val (cons val-1 val-2))]
                           [_ (error 'cons "Bad args: ~s" vals)])))


  (add-primitive-proc! 'box
                       (λ [vals : DenVal *] : ExpVal
                         (match vals
                           [`(,val-1) (box-val (box val-1))]
                           [_ (error 'box "Bad args: ~s" vals)])))

  (add-primitive-proc! 'set-box!
                       (λ [vals : DenVal *] : ExpVal
                         (match vals
                           [`(,val-1 ,val-2) #:when (denbox? val-1) (set-box! val-1 val-2)]
                           [_ (error 'set-box! "Bad args: ~s" vals)])))

  (add-primitive-proc! 'vector-set!
                       (λ [vals : DenVal *] : ExpVal
                         (match vals
                           [`(,val-1 ,val-2 ,val-3)
                            #:when (and (denvector? val-1)
                                        (index? val-2))
                            (vector-set! val-1 val-2 val-3)]
                           [_ (error 'vector-set! "Bad args: ~s" vals)])))


  (add-primitive-proc! '+ (n-ary-arithmetic-func '+ +))
  (add-primitive-proc! '* (n-ary-arithmetic-func '* *))
  (add-primitive-proc! '- (n-ary-arithmetic-func '- -))
  (add-primitive-proc! '/ (n-ary-arithmetic-func '/ /))

  (add-primitive-proc! 'void             (λ [vals : DenVal *] : ExpVal (void)))
  (add-primitive-proc! 'list             (λ [vals : DenVal *] : ExpVal (list-val vals)))
  (add-primitive-proc! 'vector           (λ [vals : DenVal *] : ExpVal (apply vector vals)))
  (add-primitive-proc! 'vector-immutable (λ [vals : DenVal *] : ExpVal (apply vector-immutable vals)))


  (add-primitive-proc! 'eq?    (binary-equal-relation 'eq?    eq?))
  (add-primitive-proc! 'eqv?   (binary-equal-relation 'eqv?   eqv?))
  (add-primitive-proc! 'equal? (binary-equal-relation 'equal? equal?))

  (add-primitive-proc! 'hash             (binary-imhash 'hash         (inst hash         DenVal DenVal)))
  (add-primitive-proc! 'hasheq           (binary-imhash 'hasheq       (inst hasheq       DenVal DenVal)))
  (add-primitive-proc! 'hasheqv          (binary-imhash 'hasheqv      (inst hasheqv      DenVal DenVal)))
  (add-primitive-proc! 'make-hash        (unary-mhash   'make-hash    (inst make-hash    DenVal DenVal)))
  (add-primitive-proc! 'make-hasheq      (unary-mhash   'make-hasheq  (inst make-hasheq  DenVal DenVal)))
  (add-primitive-proc! 'make-hasheqv     (unary-mhash   'make-hasheqv (inst make-hasheqv DenVal DenVal)))
  (add-primitive-proc! 'hash-has-key?
                       (λ [vals : DenVal *] : ExpVal
                         (match vals
                           [`(,ht ,key) #:when (denhash? ht) (hash-has-key? ht key)]
                           [_ (error 'hash-has-key?  "Bad args: ~s" vals)])))
  (add-primitive-proc! 'hash-set
                       (λ [vals : DenVal *] : ExpVal
                         (match vals
                           [`(,ht ,key ,val) #:when (denimhash? ht) (hash-set  ht key val)]
                           [_ (error 'hash-set  "Bad args: ~s" vals)])))
  (add-primitive-proc! 'hash-set!
                       (λ [vals : DenVal *] : ExpVal
                         (match vals
                           [`(,ht ,key ,val) #:when (denmhash? ht)  (hash-set! ht key val)]
                           [_ (error 'hash-set! "Bad args: ~s" vals)])))
  (add-primitive-proc! 'hash-remove
                       (λ [vals : DenVal *] : ExpVal
                         (match vals
                           [`(,ht ,key) #:when (denimhash? ht) (hash-remove  ht key)]
                           [_ (error 'hash-remove  "Bad args: ~s" vals)])))
  (add-primitive-proc! 'hash-remove!
                       (λ [vals : DenVal *] : ExpVal
                         (match vals
                           [`(,ht ,key) #:when (denmhash? ht)  (hash-remove! ht key)]
                           [_ (error 'hash-remove! "Bad args: ~s" vals)])))
  (add-primitive-proc! 'hash-clear
                       (λ [vals : DenVal *] : ExpVal
                         (match vals
                           [`(,ht) #:when (denimhash? ht) (hash-clear  ht)]
                           [_ (error 'hash-clear  "Bad args: ~s" vals)])))
  (add-primitive-proc! 'hash-clear!
                       (λ [vals : DenVal *] : ExpVal
                         (match vals
                           [`(,ht) #:when (denmhash? ht)  (hash-clear! ht)]
                           [_ (error 'hash-clear! "Bad args: ~s" vals)])))
  (add-primitive-proc! 'format
                       (λ [vals : DenVal *] : ExpVal
                         (match vals
                           [`(,str ,args ...)
                            #:when (string? str)
                            (string-val (apply format str args))]
                           [_ (error 'format "Bad args: ~s" vals)])))


  (add-primitive-proc! 'enqueue
                       (λ [vals : DenVal *] : ExpVal
                         (match vals
                           [`(,val-1 ,val-2)
                            #:when ((queueof? denval?) val-1)
                            (queue-val (enqueue val-1 val-2))]
                           [_ (error 'enqueue "Bad args: ~s" vals)])))

  (add-denval! 'dequeue
               (expval->denval
                (+eval+ '(λ (q f)
                           (cond
                             [(empty-queue? q)
                              (raise "ERROR! Passing empty queue to dequeue!")]
                             [(null? (cadr q))
                              (let ([l (reverse (car q))])
                                (f (car l) (list null (cdr l))))]
                             [else
                              (let ([l (cadr q)])
                                (f (car l) (list (car q) (cdr l))))]))
                        (base-env))))


  (add-denval! 'map undefined)
  (add-denval! 'map
               (expval->denval
                (+eval+ '(λ (func ls)
                           (if (null? ls)
                               '()
                               (cons (func (car ls)) (map func (cdr ls)))))
                        (base-env))))

  (add-denval! 'Y
               (expval->denval
                (+eval+ '(λ (f)
                           ((λ (recur-func)
                              (recur-func recur-func))
                            (λ (recur-func)
                              (f (λ args
                                   (apply (recur-func recur-func) args))))))
                        (base-env))))

  (add-denval! 'Y*
               (expval->denval
                (+eval+ '(λ funcs
                           ((λ (recur-funcs)
                              (recur-funcs recur-funcs))
                            (λ (recur-funcs)
                              (map (λ (func)
                                     (λ args
                                       (apply (apply func (recur-funcs recur-funcs)) args)))
                                   funcs))))
                        (base-env))))

  (add-denval! 'call/cc
               (expval->denval
                (+eval+ '(λ (cont) (let/cc cc (cont cc)))
                        (base-env))))


  ;; ------------------------------
  ;; Thread
  (add-denval! 'make-thd
               (expval->denval
                (+eval+ '(λ (ptid tid mail time-slice thunk)
                           (vector-immutable 'thd ptid tid mail time-slice thunk))
                        (base-env))))
  (add-denval! 'thd?
               (expval->denval
                (+eval+ '(λ (arg)
                           (and (vector? arg)
                                (immutable? arg)
                                (eqv? 'thd (vector-ref arg 0))))
                        (base-env))))

  (add-denval! 'thd-ptid       (expval->denval (+eval+ '(λ (arg) (if (thd? arg) (vector-ref arg 1) #f)) (base-env))))
  (add-denval! 'thd-tid        (expval->denval (+eval+ '(λ (arg) (if (thd? arg) (vector-ref arg 2) #f)) (base-env))))
  (add-denval! 'thd-mail       (expval->denval (+eval+ '(λ (arg) (if (thd? arg) (vector-ref arg 3) #f)) (base-env))))
  (add-denval! 'thd-time-slice (expval->denval (+eval+ '(λ (arg) (if (thd? arg) (vector-ref arg 4) #f)) (base-env))))
  (add-denval! 'thd-thunk      (expval->denval (+eval+ '(λ (arg) (if (thd? arg) (vector-ref arg 5) #f)) (base-env))))


  (add-denval! 'thread-table (make-hasheq (list (cons 0 #t))))
  (add-denval! 'mail (box (empty-queue)))
  (add-denval! 'ptid 0)
  (add-denval! 'tid  0)
  (add-denval! 'ntid 0)

  (add-denval!
   'initialize-thread-identifier!
   (expval->denval
    (+eval+
     '(λ ()
        (hash-clear! thread-table)
        (hash-set! thread-table 0 #t)
        (set-box! mail (empty-queue))
        (set! ptid 0)
        (set! tid  0)
        (set! ntid 0))
     (base-env))))

  (add-denval!
   'update-thread-identifier!
   (expval->denval
    (+eval+
     '(λ (th)
        (set! ptid (thd-ptid th))
        (set! tid  (thd-tid  th))
        (set! mail (thd-mail th))
        (hash-set! thread-table tid #t))
     (base-env))))


  (add-denval! 'get-mail (expval->denval (+eval+ '(λ () mail) (base-env))))
  (add-denval! 'get-ptid (expval->denval (+eval+ '(λ () ptid) (base-env))))
  (add-denval! 'get-tid  (expval->denval (+eval+ '(λ () tid)  (base-env))))
  (add-denval! 'get-ntid (expval->denval (+eval+ '(λ () (set! ntid (add1 ntid)) ntid) (base-env))))

  (add-denval! 'has-thread? (+eval+ '(λ (tid) (hash-has-key? thread-table tid))   (base-env)))
  (add-denval! 'get-thread  (+eval+ '(λ (tid) (hash-ref thread-table tid #f))     (base-env)))
  (add-denval! 'add-thread! (+eval+ '(λ (tid th) (hash-set! thread-table tid th)) (base-env)))


  (add-denval! 'apply-thd (+eval+ '(λ (th) (update-thread-identifier! th) ((thd-thunk th))) (base-env)))


  (add-denval! 'the-ready-queue    (empty-queue))
  (add-denval! 'the-final-answer   undefined)
  (add-denval! 'the-max-time-slice 1)
  (add-denval! 'the-time-remaining 0)

  (add-denval!
   'initialize-scheduler!
   (expval->denval
    (+eval+
     '(λ (ticks)
        (set! the-ready-queue    (empty-queue))
        (set! the-final-answer   undefined)
        (set! the-max-time-slice ticks)
        (set! the-time-remaining the-max-time-slice))
     (base-env))))


  (add-denval!
   'place-on-thread-queue
   (expval->denval
    (+eval+
     '(λ (thds thk ptid tid mail)
        (add-thread! tid
                     (make-thd ptid tid mail
                               (let ([the-time the-time-remaining])
                                 (if (exact-positive-integer? the-time)
                                     the-time
                                     the-max-time-slice))
                               thk))
        (enqueue thds tid))
     (base-env))))

  (add-denval!
   'place-on-ready-queue!
   (expval->denval
    (+eval+
     '(λ (thk ptid tid mail)
        (set! the-ready-queue
              (place-on-thread-queue
               the-ready-queue
               thk ptid tid mail)))
     (base-env))))


  (add-denval! 'set-final-answer! (+eval+ '(λ (val) (set! the-final-answer val)) (base-env)))
  (add-denval! 'time-expired?     (+eval+ '(λ () (= 0 the-time-remaining)) (base-env)))
  (add-denval! 'decrement-timer!  (+eval+ '(λ () (set! the-time-remaining (sub1 the-time-remaining))) (base-env)))

  (add-denval! 'run-next-thread undefined)
  (add-denval!
   'run-next-thread
   (expval->denval
    (+eval+
     '(λ ()
        (exit
         (if (empty-queue? the-ready-queue)
             the-final-answer
             (dequeue the-ready-queue
                      (λ (1st-ready-tid other-ready-tids)
                        (let ([th (get-thread 1st-ready-tid)])
                          (set! the-ready-queue other-ready-tids)
                          (when (thd? th)
                            (set! the-time-remaining (thd-time-slice th))
                            (let ([res (apply-thd th)])
                              (when (= 1 (get-tid))
                                (set-final-answer! res))))
                          (run-next-thread)))))))
     (base-env))))


  (add-denval!
   'kill-thread
   (expval->denval
    (+eval+
     '(λ (tid)
        (let ([th (get-thread tid)])
          (cond [(false? th) #f]
                [else
                 (hash-remove! thread-table tid)
                 (if (thd? th) #t (void))])))
     (base-env))))

  (add-denval!
   'thread-send
   (expval->denval
    (+eval+
     '(λ (tid v)
        (if (has-thread? tid)
            (let* ([th (get-thread tid)]
                   [mail (if (thd? th) (thd-mail th) (get-mail))])
              (set-box! mail (enqueue (unbox mail) v)))
            (get-tid)))
     (base-env))))

  (add-denval!
   'thread-try-receive
   (expval->denval
    (+eval+
     '(λ ()
        (let* ([mail (get-mail)]
               [valq (unbox mail)])
          (if (empty-queue? valq)
              #f
              (dequeue valq
                       (λ (1st-v other-vs)
                         (set-box! mail other-vs)
                         1st-v)))))
     (base-env))))

  (add-denval! 'thread-receive undefined)
  (add-denval!
   'thread-receive
   (expval->denval
    (+eval+
     '(λ ()
        (cond [(empty-queue? (unbox (get-mail)))
               (place-on-ready-queue!
                (λ () (thread-receive))
                (get-ptid) (get-tid) (get-mail))
               (run-next-thread)]
              [else (thread-try-receive)]))
     (base-env))))


  (add-denval!
   'spawn
   (expval->denval
    (+eval+
     '(λ (ctx)
        (let ([ctx (new-closure ctx)]
              [spawn-tid (get-ntid)])
          (let ([spawn-thk (λ () (ctx spawn-tid))])
            (place-on-ready-queue! spawn-thk (get-tid) spawn-tid (box (empty-queue))))))
     (base-env))))

  (add-denval!
   'yield
   (expval->denval
    (+eval+
     '(λ ()
        (let/cc cc
          (spawn (λ (_) (cc (get-tid))))
          (run-next-thread)))
     (base-env))))


  (add-denval!
   'make-mutex
   (expval->denval
    (+eval+
     '(λ (keys wait-queue)
        (vector 'mutex keys wait-queue))
     (base-env))))

  (add-denval!
   'mutex?
   (expval->denval
    (+eval+
     '(λ (arg)
        (and (vector? arg)
             (not (immutable? arg))
             (eqv? 'mutex (vector-ref arg 0))))
     (base-env))))

  (add-denval! 'mutex-keys       (expval->denval (+eval+ '(λ (mut) (if (mutex? mut) (vector-ref mut 1) #f)) (base-env))))
  (add-denval! 'mutex-wait-queue (expval->denval (+eval+ '(λ (mut) (if (mutex? mut) (vector-ref mut 2) #f)) (base-env))))

  (add-denval! 'set-mutex-keys!       (expval->denval (+eval+ '(λ (mut keys) (if (mutex? mut) (vector-set! mut 1 keys) #f)) (base-env))))
  (add-denval! 'set-mutex-wait-queue! (expval->denval (+eval+ '(λ (mut waqu) (if (mutex? mut) (vector-set! mut 2 waqu) #f)) (base-env))))

  (add-denval! 'mutex (expval->denval (+eval+ '(λ (keys) (make-mutex keys (empty-queue))) (base-env))))

  (add-denval!
   'wait
   (expval->denval
    (+eval+
     '(λ (mut)
        (let/cc cc
          (let ([thk (λ () (cc (void)))]
                [keys (mutex-keys mut)]
                [wait-queue (mutex-wait-queue mut)])
            (cond [(zero? keys)
                   (set-mutex-wait-queue!
                    mut
                    (place-on-thread-queue
                     wait-queue
                     thk (get-ptid) (get-tid) (get-mail)))
                   (run-next-thread)]
                  [else
                   (set-mutex-keys! mut (sub1 keys))
                   (thk)]))))
     (base-env))))

  (add-denval!
   'signal
   (expval->denval
    (+eval+
     '(λ (mut)
        (let/cc cc
          (let ([thk (λ () (cc (void)))]
                [keys (mutex-keys mut)]
                [wait-queue (mutex-wait-queue mut)])
            (if (empty-queue? wait-queue)
                (set-mutex-keys! mut (add1 keys))
                (dequeue wait-queue
                         (λ (1st-waiting-tid other-waiting-tids)
                           (set! the-ready-queue (enqueue the-ready-queue 1st-waiting-tid))
                           (set-mutex-wait-queue! mut other-waiting-tids)))))))
     (base-env))))


  (add-denval!
   'apply
   (expval->denval
    (~eval~
     '(λ (func args)
        (let/cc return
          (cond [(time-expired?)
                 (place-on-ready-queue!
                  (λ () (return (apply func args)))
                  (get-ptid) (get-tid) (get-mail))
                 (run-next-thread)]
                [else
                 (decrement-timer!)
                 (return (~apply~ func args))])))
     (extend-env '~apply~ (apply-env (base-env) 'apply) (base-env)))))

  )
