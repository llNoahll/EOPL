#lang typed/racket

(require "../types/types.rkt"
         "../ExpValues/values-sig.rkt"
         "../Environment/env-sig.rkt"
         "../Procedure/proc-sig.rkt"
         "../PrimitiveProc/primitive-proc-sig.rkt"
         "exp-sig.rkt")

(provide exp@)


(define-unit exp@
  (import values^ env^ proc^ primitive-proc^)
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
            [(var-exp? exp) (apply-env env (var-exp-var exp))]
            [(let-exp? exp)
             (let ([vals (map (λ ([bound-exp : Exp]) : DenVal
                                  (cast (value-of bound-exp env) DenVal))
                              (let-exp-bound-exps exp))])
               (value-of (let-exp-body exp)
                         (extend-env* (let-exp-bound-vars exp)
                                      vals
                                      env)))]

            [(primitive-proc-exp? exp)
             (let ([vals (map (λ ([exp : Exp]) : DenVal
                                  (cast (value-of exp env) DenVal))
                              (primitive-proc-exp-exps exp))])
               (apply (hash-ref primitive-proc-table (primitive-proc-exp-op exp))
                      vals))]
            [(proc-exp? exp)
             (proc-val (procedure (proc-exp-vars exp) (proc-exp-body exp) env))]
            [(call-exp? exp)
             (let ([proc (expval->proc (value-of (call-exp-rator exp) env))]
                   [args (map (λ ([exp : Exp]) : DenVal
                                  (cast (value-of exp env) DenVal))
                              (call-exp-rands exp))])
               (apply-procedure proc args))]

            [else (raise-argument-error 'value-of "exp?" exp)])))

  )
