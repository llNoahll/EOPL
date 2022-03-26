#lang typed/racket

(require "../types/types.rkt"
         "../Parse/parse.rkt"
         "../Reference/ref-sig.rkt"
         "../Reference/ref-unit.rkt"
         "../Continuation/cont-sig.rkt"
         "../Continuation/cont-unit.rkt"
         "../Thread/thd-sig.rkt"
         "../Thread/thd-unit.rkt"
         "../Scheduler/sche-sig.rkt"
         "../Scheduler/sche-unit.rkt"
         "../Mutex/mut-sig.rkt"
         "../Mutex/mut-unit.rkt"
         "../ExpValues/values-sig.rkt"
         "../ExpValues/values-unit.rkt"
         "../Procedure/proc-sig.rkt"
         "../Procedure/proc-unit.rkt"
         "../Environment/env-sig.rkt"
         "../Environment/env-unit.rkt"
         "../Expressions/exp-sig.rkt"
         "../Expressions/exp-unit.rkt")

(provide (all-from-out "../types/types.rkt")
         (all-from-out "../Parse/parse.rkt")
         (all-defined-out))


(define-compound-unit/infer base@
  (import)
  (export ref^ cont^ thd^ sche^ mut^ values^ env^ proc^ exp^)
  (link   ref@ cont@ thd@ sche@ mut@ values@ env@ proc@ exp@))

(define-values/invoke-unit base@
  (import)
  (export ref^ cont^ thd^ sche^ mut^ values^ env^ proc^ exp^))


(define-namespace-anchor ns-anchor)
(define eval-ns (namespace-anchor->namespace ns-anchor))

(: *eval* [->* (S-Exp Env Cont) (Exact-Positive-Integer) ExpVal])
(define *eval*
  (λ (code env cont [timeslice 1])
    (: exp Exp)
    (define exp
      (assert (call-with-values
               (λ () (eval (parse code) eval-ns))
               (λ args (car args)))
              exp?))


    ;; (pretty-print code)
    (initialize-scheduler! timeslice)
    (initialize-thread-identifier!)
    (value-of/k exp env cont)))

(let ()
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
      (if (has-binding? (base-env) name)
          (set-binding! (base-env) name val)
          (base-env (extend-env name val (base-env))))))


  (add-denval! 'undefined undefined)
  (add-denval! 'null      null)
  (add-denval! 'empty     empty)
  (add-denval! 'true      true)
  (add-denval! 'false     false)


  (add-primitive-proc! 'eval
                       (λ [vals : DenVal *] : ExpVal
                         (match vals
                           [`(,code)
                            #:when (s-exp? code)
                            (*eval* code (base-env) (id-cont))]
                           [_ (error 'eval "Bad args: ~s" vals)])))

  (add-primitive-proc! 'identity (unary-func 'identity identity))

  (add-primitive-proc! 'get-nid  (nullary-func 'get-nid  get-nid))
  (add-primitive-proc! 'get-tid  (nullary-func 'get-tid  get-tid))
  (add-primitive-proc! 'get-ptid (nullary-func 'get-ptid get-ptid))

  (add-primitive-proc! 'empty-queue (nullary-func 'empty-queue empty-queue))
  (add-primitive-proc! 'empty-list  (λ [vals : DenVal *] : ExpVal '()))

  (add-primitive-proc! 'boolean?   (unary-pred 'boolean? boolean?))
  (add-primitive-proc! 'real?      (unary-pred 'real? real?))
  (add-primitive-proc! 'char?      (unary-pred 'char? char?))
  (add-primitive-proc! 'string?    (unary-pred 'string? string?))
  (add-primitive-proc! 'symbol?    (unary-pred 'symbol? symbol?))
  (add-primitive-proc! 'undefined? (unary-pred 'undefined? undefined?))
  (add-primitive-proc! 'void?      (unary-pred 'void? void?))
  (add-primitive-proc! 'mutex?     (unary-pred 'mutex? mutex?))

  (add-primitive-proc! 'not    (unary-pred 'not not))
  (add-primitive-proc! 'false? (unary-pred 'false? false?))
  (add-primitive-proc! 'true?  (unary-pred 'true? true?))

  (add-primitive-proc! 'pair?        (unary-pred 'pair? pair?))
  (add-primitive-proc! 'null?        (unary-pred 'null? null?))
  (add-primitive-proc! 'list?        (unary-pred 'list? list?))
  (add-primitive-proc! 'empty-queue? (unary-pred 'empty-queue? empty-queue?))
  (add-primitive-proc! 'queue?       (unary-pred 'queue? queue?))

  (add-primitive-proc! 'zero? (unary-arithmetic-pred 'zero? zero?))
  (add-primitive-proc! 'sub1  (unary-arithmetic-func 'sub1 sub1))
  (add-primitive-proc! 'add1  (unary-arithmetic-func 'add1 add1))

  (add-primitive-proc! 'raise
                       (λ [vals : DenVal *] : ExpVal
                         (match vals
                           [`(,val) (error "uncaught exception:" (expval->denval val))]
                           [_ (error 'raise "Bad args: ~s" vals)])))
  (add-primitive-proc! 'car
                       (λ [vals : DenVal *] : ExpVal
                         (match vals
                           [`(,val) (car (expval->pair val))]
                           [_ (error 'car "Bad args: ~s" vals)])))
  (add-primitive-proc! 'cdr
                       (λ [vals : DenVal *] : ExpVal
                         (match vals
                           [`(,val) (cdr (expval->pair val))]
                           [_ (error 'cdr "Bad args: ~s" vals)])))

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
                       (*eval* `(λ (arg)
                                  (,(get-op curr)
                                   (,(get-op prev)
                                    arg)))
                               (base-env)
                               (id-cont))))
         now))))
  (add-primitive-proc! 'length
                       (λ [vals : DenVal *] : ExpVal
                         (match vals
                           [`(,val) (length (expval->list val))]
                           [_ (error 'length "Bad args: ~s" vals)])))
  (add-primitive-proc! 'list-ref
                       (λ [vals : DenVal *] : ExpVal
                         (match vals
                           [`(,val-1 ,val-2)
                            #:when (exact-integer? val-2)
                            (list-ref (expval->list val-1) val-2)]
                           [_ (error 'list-ref "Bad args: ~s" vals)])))


  (add-primitive-proc! 'read (nullary-func 'read read))

  (add-primitive-proc! 'display (unary-func 'display display))
  (add-primitive-proc! 'print   (unary-func 'print   print))
  (add-primitive-proc! 'write   (unary-func 'write   write))

  (add-primitive-proc! 'displayln (unary-func 'displayln displayln))
  (add-primitive-proc! 'println   (unary-func 'println   println))
  (add-primitive-proc! 'writeln   (unary-func 'writeln   writeln))


  (add-primitive-proc! '=  (binary-arithmetic-relation '=  =))
  (add-primitive-proc! '>  (binary-arithmetic-relation '>  >))
  (add-primitive-proc! '>= (binary-arithmetic-relation '>= >=))
  (add-primitive-proc! '<  (binary-arithmetic-relation '<  <))
  (add-primitive-proc! '<= (binary-arithmetic-relation '<= <=))

  (add-primitive-proc! 'eq?    (binary-equal-relation 'eq?    eq?))
  (add-primitive-proc! 'eqv?   (binary-equal-relation 'eqv?   eqv?))
  (add-primitive-proc! 'equal? (binary-equal-relation 'equal? equal?))


  (add-primitive-proc! 'cons
                       (λ [vals : DenVal *] : ExpVal
                         (match vals
                           [`(,val-1 ,val-2) (pair-val (cons val-1 val-2))]
                           [_ (error 'cons "Bad args: ~s" vals)])))

  (add-primitive-proc! 'enqueue
                       (λ [vals : DenVal *] : ExpVal
                         (match vals
                           [`(,val-1 ,val-2)
                            #:when ((queueof? denval?) val-1)
                            (queue-val (enqueue val-1 val-2))]
                           [_ (error 'enqueue "Bad args: ~s" vals)])))

  (add-primitive-proc! 'dequeue
                       (λ [vals : DenVal *] : ExpVal
                         (match vals
                           [`(,val-1 ,val-2)
                            #:when (and ((queueof? denval?) val-1)
                                        (proc? val-2))
                            (dequeue val-1
                                     (ann (λ (1st others)
                                            (apply-procedure/k val-2 (list 1st others) (id-cont)))
                                          [-> DenVal (Queueof DenVal) DenVal]))]
                           [_ (error 'dequeue "Bad args: ~s" vals)])))

  (add-primitive-proc! '+ (n-ary-arithmetic-func '+ +))
  (add-primitive-proc! '* (n-ary-arithmetic-func '* *))
  (add-primitive-proc! '- (n-ary-arithmetic-func '- -))
  (add-primitive-proc! '/ (n-ary-arithmetic-func '/ /))

  (add-primitive-proc! 'void
                       (λ [vals : DenVal *] : ExpVal (void)))
  (add-primitive-proc! 'list
                       (λ [vals : DenVal *] : ExpVal (list-val vals)))
  (add-primitive-proc! 'format
                       (λ [vals : DenVal *] : ExpVal
                         (match vals
                           [`(,str ,args ...)
                            #:when (string? str)
                            (string-val (apply format str args))]
                           [_ (error 'format "Bad args: ~s" vals)])))


  (add-denval! 'apply
               (proc-val
                (procedure '(k)
                           (proc-exp '(func args)
                                     (call-exp (call-exp (var-exp 'func)
                                                         (list (var-exp 'k)))
                                               (var-exp 'args)))
                           (empty-env))))

  (add-denval! 'Y
               (expval->denval
                (*eval* '(λ (f)
                           ((λ (recur-func)
                              (recur-func recur-func))
                            (λ (recur-func)
                              (f (λ args
                                   (apply (recur-func recur-func) args))))))
                        (base-env)
                        (id-cont))))


  (add-denval! 'map undefined)
  (add-denval! 'map
               (expval->denval
                (*eval* '(λ (func ls)
                           (if (null? ls)
                               '()
                               (cons (func (car ls))
                                     (map func (cdr ls)))))
                        (base-env)
                        (id-cont))))

  (add-denval! 'Y*
               (expval->denval
                (*eval* '(λ funcs
                           ((λ (recur-funcs)
                              (recur-funcs recur-funcs))
                            (λ (recur-funcs)
                              (map (λ (func)
                                     (λ args
                                       (apply (apply func (recur-funcs recur-funcs)) args)))
                                   funcs))))
                        (base-env)
                        (id-cont))))

  (add-denval! 'call/cc
               (expval->denval
                (*eval* '(λ (cont) (let/cc cc (cont cc)))
                        (base-env)
                        (id-cont))))

  )
