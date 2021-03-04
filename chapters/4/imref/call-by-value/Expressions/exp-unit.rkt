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

  (: value-of [-> Exp Env ExpVal])
  (define value-of
    (λ (exp env)
      (match exp
        [(assign-exp var exp)
         (set-binding! env var (expval->denval (value-of exp env)))]

        [(symbol-exp sym) (symbol-val sym)]
        [(const-exp num) (num-val num)]
        [(bool-exp bool) (bool-val bool)]
        [(char-exp char) (char-val char)]
        [(string-exp str) (string-val str)]

        [(var-exp var) (apply-env env var)]

        [(begin-exp exps)
         (define expvals
           (map (ann (λ (exp) (value-of exp env)) [-> Exp ExpVal])
                exps))

         (car (last-pair expvals))]

        [(if-exp pred-exp true-exp false-exp)
         (define pred-val (value-of pred-exp env))

         (value-of (if (expval->bool pred-val)
                       true-exp
                       false-exp)
                   env)]
        [(cond-exp branches)
         (define branch
           (assf (λ ([pred-exp : Exp]) : Boolean
                   (true? (value-of pred-exp env)))
                 branches))

         (if (false? branch)
             (void)
             (value-of (cadr branch) env))]

        [(let-exp vars exps body)
         (define vals
           (map (ann (λ (bind-exp) (expval->denval (value-of bind-exp env)))
                     [-> Exp DenVal])
                exps))

         (value-of body (extend-env* vars vals env))]
        [(letrec-exp vars exps body) (value-of body (extend-env-rec* vars exps env))]

        [(primitive-proc-exp op exps)
         (define vals
           (map (ann (λ (exp) (expval->denval (value-of exp env)))
                     [-> Exp DenVal])
                exps))

         (apply (hash-ref primitive-proc-table op) vals)]
        [(trace-proc-exp vars body) (proc-val (trace-procedure vars body env))]
        [(proc-exp vars body) (proc-val (procedure vars body env))]
        [(call-exp rator rands)
         (define proc (expval->proc (value-of rator env)))

         (if (var-exp? rands)
             (apply-procedure proc (cast (value-of rands env) (Listof DenVal)))
             (let ([args (map (ann (λ (exp) (expval->denval (value-of exp env)))
                                   [-> Exp DenVal])
                              rands)])
               (apply-procedure proc args)))])))

  )
