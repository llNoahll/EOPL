#lang typed/racket

(require "../types/types.rkt"
         "../ExpValues/values-sig.rkt"
         "../Environment/env-sig.rkt"
         "../Procedure/proc-sig.rkt"
         "primitive-proc-sig.rkt")

(provide primitive-proc@)


(define-unit primitive-proc@
  (import values^ env^ proc^)
  (export primitive-proc^)

  (: base-env (Parameter Env))
  (define base-env (make-parameter (empty-env)))


  (: primitive-proc-table (Mutable-HashTable Symbol [-> DenVal * ExpVal]))
  (define primitive-proc-table (make-hasheqv))

  (: unary-arithmetic-pred [-> [-> Real Boolean] [-> DenVal * ExpVal]])
  (define unary-arithmetic-pred
    (λ (pred)
      (λ vals
        (match vals
          [`(,val) (bool-val (pred (expval->num val)))]
          [_ (error 'unary-pred "Bad args: ~s" vals)]))))

  (: unary-arithmetic-func [-> [-> Real Real] [-> DenVal * ExpVal]])
  (define unary-arithmetic-func
    (λ (func)
      (λ vals
        (match vals
          [`(,val) (num-val (func (expval->num val)))]
          [_ (error 'unary-func "Bad args: ~s" vals)]))))

  (: unary-IO-func [-> [-> Any Void] [-> DenVal * ExpVal]])
  (define unary-IO-func
    (λ (func)
      (λ vals
        (match vals
          [`(,val) (func (expval->s-expval val))]
          [_ (error 'unary-func "Bad args: ~s" vals)]))))


  (: binary-equal-relation [-> [-> Any Any Boolean] [-> DenVal * ExpVal]])
  (define binary-equal-relation
    (λ (relation)
      (λ vals
        (match vals
          [`(,val-1 ,val-2)
           (bool-val (relation (expval->s-expval val-1)
                               (expval->s-expval val-2)))]
          [_ (error 'binary-relation "Bad args: ~s" vals)]))))


  (: binary-arithmetic-relation [-> [-> Real Real Boolean] [-> DenVal * ExpVal]])
  (define binary-arithmetic-relation
    (λ (relation)
      (λ vals
        (match vals
          [`(,val-1 ,val-2)
           (bool-val (relation (expval->num val-1)
                               (expval->num val-2)))]
          [_ (error 'binary-relation "Bad args: ~s" vals)]))))


  (: n-ary-arithmetic-func [-> [-> Real Real * Real] [-> DenVal * ExpVal]])
  (define n-ary-arithmetic-func
    (λ (func)
      (λ vals
        (match vals
          [`(,val-1 . ,(? list? vals))
           (num-val (apply func
                           (expval->num val-1)
                           (map (λ ([val : DenVal]) : Real
                                  (expval->num val))
                                vals)))]
          [_ (error 'n-ary-arithmetic "Bad args: ~s" vals)]))))


  (: add-primitive-proc! [-> Symbol [-> DenVal * ExpVal] Void])
  (define add-primitive-proc!
    (λ (op-name op)
      (hash-set! primitive-proc-table op-name op)
      (base-env (extend-env op-name
                            (proc-val (procedure 'args
                                                 (primitive-proc-exp 'apply-primitive
                                                                     (list (symbol-exp op-name)
                                                                           (var-exp 'args)))
                                                 (empty-env)))
                            (base-env)))))


  (add-primitive-proc! 'empty-list (λ [vals : DenVal *] : ExpVal '()))


  (add-primitive-proc! 'zero? (unary-arithmetic-pred zero?))
  (add-primitive-proc! 'minus (unary-arithmetic-func -))
  (add-primitive-proc! 'add   (unary-arithmetic-func +))
  (add-primitive-proc! 'car (λ [vals : DenVal *] : ExpVal
                                (match vals
                                  [`(,val) (car (expval->pair val))]
                                  [_ (error 'unary-func "Bad args: ~s" vals)])))
  (add-primitive-proc! 'cdr (λ [vals : DenVal *] : ExpVal
                                (match vals
                                  [`(,val) (cdr (expval->pair val))]
                                  [_ (error 'unary-func "Bad args: ~s" vals)])))
  (add-primitive-proc! 'null? (λ [vals : DenVal *] : ExpVal
                                  (match vals
                                    [`(,val) (bool-val (null? val))]
                                    [_ (error 'unary-func "Bad args: ~s" vals)])))

  (add-primitive-proc! 'display (unary-IO-func display))
  (add-primitive-proc! 'print (unary-IO-func print))
  (add-primitive-proc! 'write (unary-IO-func write))

  (add-primitive-proc! 'displayln (unary-IO-func displayln))
  (add-primitive-proc! 'println (unary-IO-func println))
  (add-primitive-proc! 'writeln (unary-IO-func writeln))


  (add-primitive-proc! '= (binary-arithmetic-relation =))
  (add-primitive-proc! '> (binary-arithmetic-relation >))
  (add-primitive-proc! '< (binary-arithmetic-relation <))

  (add-primitive-proc! 'eq?    (binary-equal-relation eq?))
  (add-primitive-proc! 'eqv?   (binary-equal-relation eqv?))
  (add-primitive-proc! 'equal? (binary-equal-relation equal?))


  (add-primitive-proc! 'cons (λ [vals : DenVal *] : ExpVal
                                 (match vals
                                   [`(,val-1 ,val-2) (pair-val (cons val-1 val-2))]
                                   [_ (error 'binary-func "Bad args: ~s" vals)])))

  (add-primitive-proc! 'apply-primitive (λ [vals : DenVal *] : ExpVal
                                            (match vals
                                              [`(,(? symbol? val-1) ,(? list? val-2))
                                               (apply (hash-ref primitive-proc-table val-1) val-2)]
                                              [_ (error 'binary-func "Bad args: ~s" vals)])))


  (add-primitive-proc! '+ (n-ary-arithmetic-func +))
  (add-primitive-proc! '- (n-ary-arithmetic-func -))
  (add-primitive-proc! '* (n-ary-arithmetic-func *))
  (add-primitive-proc! '/ (n-ary-arithmetic-func /))

  (add-primitive-proc! 'list (λ [vals : DenVal *] : ExpVal (list-val vals)))

  )
