#lang typed/racket

(require "../types/types.rkt"
         "../ExpValues/values-sig.rkt"
         "../Environment/env-sig.rkt"
         "exp-sig.rkt")

(provide exp@)


(define-unit exp@
  (import values^ env^)
  (export exp^)

  (: symbol-exp [-> Symbol Symbol-Exp])
  (define symbol-exp (λ (symbol) (make-symbol-exp symbol)))

  (: const-exp [-> Integer Const-Exp])
  (define const-exp (λ (num) (make-const-exp num)))

  (: bool-exp [-> Boolean Bool-Exp])
  (define bool-exp (λ (bool) (make-bool-exp bool)))


  (: if-exp [-> Exp Exp Exp If-Exp])
  (define if-exp
    (λ (pred-exp true-exp false-exp)
      (make-if-exp pred-exp true-exp false-exp)))

  (: cond-exp [-> (Listof (Pair Exp (Listof Exp))) Cond-Exp])
  (define cond-exp
    (λ (exps)
      (make-cond-exp (ann exps (Listof (Pair Exp (Listof Exp)))))))

  (: var-exp [-> Symbol Var-Exp])
  (define var-exp (λ (var) (make-var-exp var)))

  (: let-exp [-> (Listof Symbol) (Listof Exp) Exp Let-Exp])
  (define let-exp
    (λ (bound-vars bound-exps body)
      (make-let-exp bound-vars bound-exps body)))


  (: primitive-proc-exp [-> Symbol Exp * Primitive-Proc-Exp])
  (define primitive-proc-exp (λ (op . exps) (make-primitive-proc-exp op exps)))


  (: proc-exp [-> (Listof Symbol) Exp Proc-Exp])
  (define proc-exp
    (λ (vars body)
      (make-proc-exp vars body)))

  (: call-exp [-> Exp (Listof Exp) Call-Exp])
  (define call-exp
    (λ (rator rands)
      (make-call-exp rator rands)))


  (: value-of [-> Exp Env ExpVal])
  (define value-of
    (λ (exp env)
      (cond [(symbol-exp? exp) (symbol-val (symbol-exp-symbol exp))]
            [(const-exp? exp) (num-val (const-exp-num exp))]
            [(bool-exp?  exp) (bool-val (bool-exp-bool exp))]

            [(if-exp? exp)
             (let ([pred-val (value-of (if-exp-pred-exp exp) env)])
               (if (expval->bool pred-val)
                   (value-of (if-exp-true-exp exp) env)
                   (value-of (if-exp-false-exp exp) env)))]
            [(cond-exp? exp)
             (let* ([exps (cond-exp-exps exp)]
                    [branch-exp
                     (assf (λ ([pred-exp : Exp])
                             (not (false? (value-of pred-exp env))))
                           exps)])
               (if (false? branch-exp)
                   (error 'value-of "cond-exp miss true banch!")
                   (value-of (cadr branch-exp) env)))]
            [(var-exp? exp) (cast (apply-env env (var-exp-var exp)) ExpVal)]
            [(let-exp? exp)
             (let ([vals (map (λ ([bound-exp : Exp]) : ExpVal
                                  (value-of bound-exp env))
                              (let-exp-bound-exps exp))])
               (value-of (let-exp-body exp)
                         (extend-env* (let-exp-bound-vars exp)
                                      vals
                                      env)))]

            [(primitive-proc-exp? exp)
             (let ([vals : (Listof DenVal)
                         (map (λ ([exp : Exp]) : DenVal
                                  (cast (value-of exp env) DenVal))
                              (primitive-proc-exp-exps exp))])
               (apply (hash-ref primitive-proc-table (primitive-proc-exp-op exp))
                      vals))]

            [else (raise-argument-error 'value-of "exp?" exp)])))


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


  (hash-set*! primitive-proc-table
              'empty-list (λ [vals : DenVal *] : ExpVal '())


              'zero? (unary-arithmetic-pred zero?)
              'minus (unary-arithmetic-func -)
              'add   (unary-arithmetic-func +)
              'car (λ [vals : DenVal *] : ExpVal
                     (match vals
                       [`(,val) (car (expval->pair val))]
                       [_ (error 'unary-func "Bad args: ~s" vals)]))
              'cdr (λ [vals : DenVal *] : ExpVal
                     (match vals
                       [`(,val) (cdr (expval->pair val))]
                       [_ (error 'unary-func "Bad args: ~s" vals)]))
              'null? (λ [vals : DenVal *] : ExpVal
                       (match vals
                         [`(,val) (bool-val (null? val))]
                         [_ (error 'unary-func "Bad args: ~s" vals)]))

              'display (unary-IO-func display)
              'print (unary-IO-func print)
              'write (unary-IO-func write)

              'displayln (unary-IO-func displayln)
              'println (unary-IO-func println)
              'writeln (unary-IO-func writeln)


              '+ (binary-arithmetic-func +)
              '- (binary-arithmetic-func -)
              '* (binary-arithmetic-func *)
              '/ (binary-arithmetic-func quotient)

              '= (binary-relation =)
              '> (binary-relation >)
              '< (binary-relation <)

              'cons (λ [vals : DenVal *] : ExpVal
                      (match vals
                        [`(,val-1 ,val-2) (pair-val (cons val-1 val-2))]
                        [_ (error 'binary-func "Bad args: ~s" vals)]))

              'apply-primitive (λ [vals : DenVal *] : ExpVal
                                 (match vals
                                   [`(,(? symbol? val-1) ,(? list? val-2))
                                    (apply (hash-ref primitive-proc-table val-1) val-2)]
                                   [_ (error 'binary-func "Bad args: ~s" vals)]))


              'list (λ [vals : DenVal *] : ExpVal (list-val vals))
              )

  )
