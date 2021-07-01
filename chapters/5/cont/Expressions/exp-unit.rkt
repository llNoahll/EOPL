#lang typed/racket

(require "../types/types.rkt"
         "../Reference/ref-sig.rkt"
         "../Continuation/cont-sig.rkt"
         "../ExpValues/values-sig.rkt"
         "../Environment/env-sig.rkt"
         "../Procedure/proc-sig.rkt"
         "../PrimitiveProc/primitive-proc-sig.rkt"
         "exp-sig.rkt")

(provide exp@)


(define-unit exp@
  (import ref^ cont^ values^ env^ proc^ primitive-proc^)
  (export exp^)


  (: value-of/k [-> Exp Env Cont FinalAnswer])
  (define value-of/k
    (λ (exp env cont)
      (match exp
        [(assign-exp var exp)
         (value-of/k exp
                     env
                     (assign-cont var env cont))]
        [(symbol-exp sym) (apply-cont cont (symbol-val sym))]
        [(const-exp num) (apply-cont cont (num-val num))]
        [(bool-exp bool) (apply-cont cont (bool-val bool))]
        [(char-exp char) (apply-cont cont (char-val char))]
        [(string-exp str) (apply-cont cont (string-val str))]
        [(var-exp var) (apply-cont cont (apply-env env var))]

        [(begin-exp exps)
         (define next (cdr exps))
         (value-of/k (car exps)
                     env
                     (if (null? next)
                         cont
                         (begin-cont next env cont)))]

        [(if-exp pred-exp true-exp false-exp)
         (value-of/k pred-exp
                     env
                     (if-cont true-exp false-exp env cont))]
        [(cond-exp branches)
         (define branch (car branches))
         (value-of/k (car branch)
                     env
                     (cond-cont (cadr branch) (cdr branches) env cont))]

        [(let-exp vars exps body)
         (cond [(or (null? vars) (null? exps)) (value-of/k body env cont)]
               [else
                (value-of/k (car exps)
                            env
                            (let-cont (cdr exps) vars body env cont))])]
        [(letrec-exp vars exps body)
         (cond [(or (null? vars) (null? exps)) (value-of/k body env cont)]
               [else
                (define new-env
                  (extend-env+ (map (ann (λ (var) (cons var undefined))
                                         [-> Symbol (Pair Symbol Undefined)])
                                    vars)
                               env))
                (value-of/k (car exps)
                            new-env
                            (letrec-cont (car vars) (cdr vars) (cdr exps) body new-env cont))])]

        [(primitive-proc-exp op exps)
         (cond [(null? exps) (apply-cont cont ((hash-ref primitive-proc-table op)))]
               [else
                (value-of/k (car exps)
                            env
                            (primitive-proc-cont op (cdr exps) env cont))])]
        [(trace-proc-exp vars body) (apply-cont cont (proc-val (trace-procedure vars body env)))]
        [(proc-exp vars body) (apply-cont cont (proc-val (procedure vars body env)))]
        [(call-exp rator rands) (value-of/k rator env (rator-cont rands env cont))])))

  )
