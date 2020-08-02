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

  (: unary-arithmetic-pred [-> [-> Integer Boolean] [-> DenVal * ExpVal]])
  (define unary-arithmetic-pred
    (λ (pred)
      (λ vals
        (match vals
          [`(,val) (bool-val (pred (expval->num val)))]
          [_ (error 'unary-pred "Bad args: ~s" vals)]))))

  (: unary-arithmetic-func [-> [-> Integer Integer] [-> DenVal * ExpVal]])
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


  (: binary-arithmetic-func [-> [-> Integer Integer Integer] [-> DenVal * ExpVal]])
  (define binary-arithmetic-func
    (λ (func)
      (λ vals
        (match vals
          [`(,val-1 ,val-2) (num-val (func (expval->num val-1) (expval->num val-2)))]
          [_ (error 'binary-func "Bad args: ~s" vals)]))))

  (: binary-relation [-> [-> Integer Integer Boolean] [-> DenVal * ExpVal]])
  (define binary-relation
    (λ (relation)
      (λ vals
        (match vals
          [`(,val-1 ,val-2) (bool-val (relation (expval->num val-1) (expval->num val-2)))]
          [_ (error 'binary-relation "Bad args: ~s" vals)]))))



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


  (add-primitive-proc! '+ (binary-arithmetic-func +))
  (add-primitive-proc! '- (binary-arithmetic-func -))
  (add-primitive-proc! '* (binary-arithmetic-func *))
  (add-primitive-proc! '/ (binary-arithmetic-func quotient))

  (add-primitive-proc! '= (binary-relation =))
  (add-primitive-proc! '> (binary-relation >))
  (add-primitive-proc! '< (binary-relation <))

  (add-primitive-proc! 'cons (λ [vals : DenVal *] : ExpVal
                                 (match vals
                                   [`(,val-1 ,val-2) (pair-val (cons val-1 val-2))]
                                   [_ (error 'binary-func "Bad args: ~s" vals)])))

  (add-primitive-proc! 'apply-primitive (λ [vals : DenVal *] : ExpVal
                                            (match vals
                                              [`(,(? symbol? val-1) ,(? list? val-2))
                                               (apply (hash-ref primitive-proc-table val-1) val-2)]
                                              [_ (error 'binary-func "Bad args: ~s" vals)])))


  (add-primitive-proc! 'list (λ [vals : DenVal *] : ExpVal (list-val vals)))

  )
