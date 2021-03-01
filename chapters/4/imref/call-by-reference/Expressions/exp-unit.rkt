#lang typed/racket

(require "../types/types.rkt"
         "../Reference/ref-sig.rkt"
         "../ExpValues/values-sig.rkt"
         "../Environment/env-sig.rkt"
         "../Procedure/proc-sig.rkt"
         "../PrimitiveProc/primitive-proc-sig.rkt"
         "exp-sig.rkt")

(provide exp@)


(define-unit exp@
  (import ref^ values^ env^ proc^ primitive-proc^)
  (export exp^)

  (: assign-exp [-> Symbol Exp Assign-Exp])
  (define assign-exp (λ (symbol exp) (make-assign-exp symbol exp)))


  (: symbol-exp [-> Symbol Symbol-Exp])
  (define symbol-exp (λ (symbol) (make-symbol-exp symbol)))

  (: const-exp [-> Real Const-Exp])
  (define const-exp (λ (num) (make-const-exp num)))

  (: bool-exp [-> Boolean Bool-Exp])
  (define bool-exp (λ (bool) (make-bool-exp bool)))

  (: char-exp [-> Char Char-Exp])
  (define char-exp (λ (ch) (make-char-exp ch)))

  (: string-exp [-> String String-Exp])
  (define string-exp (λ (str) (make-string-exp str)))


  (: if-exp [-> Exp Exp Exp If-Exp])
  (define if-exp
    (λ (pred-exp true-exp false-exp)
      (make-if-exp pred-exp true-exp false-exp)))

  (: cond-exp [-> (Pair (Pair Exp (Listof Exp))
                        (Listof (Pair Exp (Listof Exp))))
                  Cond-Exp])
  (define cond-exp
    (λ (exps)
      (make-cond-exp exps)))

  (: var-exp [-> Symbol Var-Exp])
  (define var-exp (λ (var) (make-var-exp var)))

  (: let-exp [-> (Listof Symbol) (Listof Exp) Exp Let-Exp])
  (define let-exp
    (λ (bind-vars bind-exps body)
      (make-let-exp bind-vars bind-exps body)))

  (: letrec-exp [-> (Listof Symbol) (Listof Exp) Exp Letrec-Exp])
  (define letrec-exp
    (λ (bind-vars bind-exps body)
      (make-letrec-exp bind-vars bind-exps body)))


  (: begin-exp [-> (Pair Exp (Listof Exp)) Begin-Exp])
  (define begin-exp
    (λ (exps)
      (make-begin-exp exps)))


  (: primitive-proc-exp [-> Symbol Exp * Primitive-Proc-Exp])
  (define primitive-proc-exp (λ (op . exps) (make-primitive-proc-exp op exps)))


  (: proc-exp [-> (U Symbol (Listof Symbol)) Exp Proc-Exp])
  (define proc-exp
    (λ (vars body)
      (make-proc-exp vars body)))

  (: trace-proc-exp [-> (U Symbol (Listof Symbol)) Exp Trace-Proc-Exp])
  (define trace-proc-exp
    (λ (vars body)
      (make-trace-proc-exp vars body)))

  (: call-exp [-> Exp (U Var-Exp (Listof Exp)) Call-Exp])
  (define call-exp
    (λ (rator rands)
      (make-call-exp rator rands)))


  (: value-of [-> Exp Env ExpVal])
  (define value-of
    (λ (exp env)
      (cond [(assign-exp? exp)
             (define ref (apply-env env (assign-exp-var exp)))
             (define val (expval->denval (value-of (assign-exp-exp exp) env)))

             (setref! ref val)]

            [(symbol-exp? exp) (symbol-val (symbol-exp-symbol exp))]
            [(const-exp? exp) (num-val (const-exp-num exp))]
            [(bool-exp? exp) (bool-val (bool-exp-bool exp))]
            [(char-exp? exp) (char-val (char-exp-char exp))]
            [(string-exp? exp) (string-val (string-exp-str exp))]

            [(begin-exp? exp)
             (define expvals
               (map (ann (λ (exp) (value-of exp env)) [-> Exp ExpVal])
                    (begin-exp-exps exp)))

             (car (last-pair expvals))]

            [(if-exp? exp)
             (define pred-val (value-of (if-exp-pred-exp exp) env))

             (if (expval->bool pred-val)
                 (value-of (if-exp-true-exp exp) env)
                 (value-of (if-exp-false-exp exp) env))]
            [(cond-exp? exp)
             (define exps (cond-exp-exps exp))
             (define branch-exp
               (assf (λ ([pred-exp : Exp])
                       (true? (value-of pred-exp env)))
                     exps))

             (if (false? branch-exp)
                 (error 'value-of "cond-exp miss true banch!")
                 (value-of (cadr branch-exp) env))]
            [(var-exp? exp) (deref (apply-env env (var-exp-var exp)))]
            [(let-exp? exp)
             (define vals
               (map (ann (λ (bind-exp) (expval->denval (value-of bind-exp env)))
                         [-> Exp DenVal])
                    (let-exp-bind-exps exp)))

             (value-of (let-exp-body exp)
                       (extend-env* (let-exp-bind-vars exp)
                                    vals
                                    env))]
            [(letrec-exp? exp)
             (value-of (letrec-exp-body exp)
                       (extend-env-rec* (letrec-exp-bind-vars exp)
                                        (letrec-exp-bind-exps exp)
                                        env))]

            [(primitive-proc-exp? exp)
             (define vals
               (map (ann (λ (exp) (expval->denval (value-of exp env)))
                         [-> Exp DenVal])
                    (primitive-proc-exp-exps exp)))

             (apply (hash-ref primitive-proc-table (primitive-proc-exp-op exp))
                    vals)]
            [(trace-proc-exp? exp)
             (proc-val (trace-procedure (proc-exp-vars exp) (proc-exp-body exp) env))]
            [(proc-exp? exp)
             (proc-val (procedure (proc-exp-vars exp) (proc-exp-body exp) env))]
            [(call-exp? exp)
             (define proc (expval->proc (value-of (call-exp-rator exp) env)))
             (define rands (call-exp-rands exp))

             (if (var-exp? rands)
                 (apply-procedure proc (cast (value-of rands env) (Listof DenVal)))
                 (let ([args (map (ann (λ (exp)
                                         (if (var-exp? exp)
                                             (apply-env env (var-exp-var exp))
                                             (expval->denval (value-of exp env))))
                                       [-> Exp (U DenVal Ref)])
                                  rands)])
                   (apply-procedure proc args)))]

            [else (raise-argument-error 'value-of "exp?" exp)])))

  )
