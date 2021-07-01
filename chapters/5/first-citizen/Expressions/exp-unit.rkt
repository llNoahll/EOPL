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
                     (ann (λ (val)
                            (apply-cont cont
                                        (set-binding! env var (expval->denval val))))
                          Cont))]
        [(symbol-exp sym) (apply-cont cont (symbol-val sym))]
        [(const-exp num) (apply-cont cont (num-val num))]
        [(bool-exp bool) (apply-cont cont (bool-val bool))]
        [(char-exp char) (apply-cont cont (char-val char))]
        [(string-exp str) (apply-cont cont (string-val str))]
        [(var-exp var) (apply-cont cont (apply-env env var))]

        [(begin-exp exps)
         (let loop : FinalAnswer ([exps exps])
           (define next (cdr exps))
           (value-of/k (car exps)
                       env
                       (if (null? next)
                           cont
                           (ann (λ (val) (loop next)) Cont))))]

        [(if-exp pred-exp true-exp false-exp)
         (value-of/k pred-exp
                     env
                     (ann (λ (pred-val)
                            (value-of/k (if (expval->bool pred-val)
                                            true-exp
                                            false-exp)
                                        env
                                        cont))
                          Cont))]
        [(cond-exp branches)
         (let loop : FinalAnswer ([branches branches])
              (cond [(null? branches) (apply-cont cont (void))]
                    [else
                     (define branch (car branches))

                     (value-of/k (car branch)
                                 env
                                 (ann (λ (pred-val)
                                        (if (expval->bool pred-val)
                                            (value-of/k (cadr branch) env cont)
                                            (loop (cdr branches))))
                                      Cont))]))]

        [(let-exp vars exps body)
         (let loop : FinalAnswer
              ([exps exps]
               [vals : (Listof DenVal) '()])
              (if (null? exps)
                  (value-of/k body
                              (extend-env* vars (reverse vals) env)
                              cont)
                  (value-of/k (car exps)
                              env
                              (ann (λ (val)
                                     (loop (cdr exps)
                                           (cons (expval->denval val) vals)))
                                   Cont))))]
        [(letrec-exp vars exps body)
         (define new-env
           (extend-env+ (map (ann (λ (var) (cons var undefined))
                                  [-> Symbol (Pair Symbol Undefined)])
                             vars)
                        env))

         (let loop : FinalAnswer ([exps exps] [vars vars])
              (if (null? exps)
                  (value-of/k body new-env cont)
                  (value-of/k (car exps)
                              new-env
                              (ann (λ (val)
                                     (set-binding! new-env (car vars) (expval->denval val))
                                     (loop (cdr exps) (cdr vars)))
                                   Cont))))]

        [(primitive-proc-exp op exps)
         (let loop : FinalAnswer
              ([exps exps]
               [vals : (Listof DenVal) '()])
              (if (null? exps)
                  (apply-cont cont
                              (apply (hash-ref primitive-proc-table op)
                                     (reverse vals)))
                  (value-of/k (car exps)
                              env
                              (ann (λ (val)
                                     (loop (cdr exps) (cons (expval->denval val) vals)))
                                   Cont))))]
        [(trace-proc-exp vars body) (apply-cont cont (proc-val (trace-procedure vars body env)))]
        [(proc-exp vars body) (apply-cont cont (proc-val (procedure vars body env)))]
        [(call-exp rator rands)
         (value-of/k rator
                   env
                   (ann (λ (val)
                          (define proc (expval->proc val))

                          (if (var-exp? rands)
                              (value-of/k rands
                                          env
                                          (ann (λ (args)
                                                 (apply-procedure/k proc (cast args (Listof DenVal)) cont))
                                               Cont))
                              (let loop : FinalAnswer
                                   ([rands rands] [args : (Listof DenVal) '()])
                                   (if (null? rands)
                                       (apply-procedure/k proc (reverse args) cont)
                                       (value-of/k (car rands)
                                                   env
                                                   (ann (λ (arg)
                                                          (loop (cdr rands)
                                                                (cons (expval->denval arg) args)))
                                                        Cont))))))
                        Cont))])))

  )
