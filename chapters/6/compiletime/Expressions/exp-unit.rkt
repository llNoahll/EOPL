#lang typed/racket

(require "../types/types.rkt"
         "../Reference/ref-sig.rkt"
         "../Continuation/cont-sig.rkt"
         "../ExpValues/values-sig.rkt"
         "../Environment/env-sig.rkt"
         "../Procedure/proc-sig.rkt"
         "exp-sig.rkt")

(provide exp@)


(define-unit exp@
  (import ref^ cont^ values^ env^ proc^)
  (export exp^)


  (: value-of/k [-> Exp Env Cont FinalAnswer])
  (define value-of/k
    (λ (exp env cont)
      (match exp
        [(assign-exp var exp)
         (value-of/k
          exp env
          (cons
           (frame
            'assign-frame
            (ann (λ (cont)
                   (λ (val)
                     (apply-cont cont (set-binding! env var (expval->denval val)))))
                 [-> Cont [-> ExpVal FinalAnswer]]))
           cont))]
        [(quote-exp  datum) (apply-cont cont (s-expval->denval datum))]
        [(symbol-exp sym)   (apply-cont cont (symbol-val sym))]
        [(real-exp   num)   (apply-cont cont (num-val    num))]
        [(bool-exp   bool)  (apply-cont cont (bool-val   bool))]
        [(char-exp   char)  (apply-cont cont (char-val   char))]
        [(string-exp str)   (apply-cont cont (string-val str))]

        [(var-exp    var)   (apply-cont cont (apply-env env var))]

        [(begin-exp exps)
         (value-of/k
          (car exps) env
          (cons
           (frame
            'begin-frame
            (ann (λ (cont)
                   (λ (val)
                     (let ([exps (cdr exps)])
                       (if (null? exps)
                           (apply-cont cont val)
                           (value-of/k (begin-exp exps) env cont)))))
                 [-> Cont [-> ExpVal FinalAnswer]]))
           cont))]

        [(if-exp pred-exp true-exp false-exp)
         (value-of/k
          pred-exp env
          (cons
           (frame
            'if-frame
            (ann (λ (cont)
                   (λ (pred-val)
                     (value-of/k
                      (if (expval->bool pred-val)
                          true-exp
                          false-exp)
                      env cont)))
                 [-> Cont [-> ExpVal FinalAnswer]]))
           cont))]

        [(new-closure-exp exp)
         (value-of/k
          exp env
          (cons
           (frame
            'new-closure-frame
            (ann (λ (cont)
                   (λ (op)
                     (cond [(proc? op)
                            (apply-cont cont
                                        (if (thread-share-memory?)
                                            op
                                            (proc (proc-vars op)
                                                  (proc-body op)
                                                  (copy-env (proc-saved-env op)))))]
                           [(primitive-proc? op) (apply-cont cont op)]
                           [else (raise-argument-error 'value-of/k "operator?" op)])))
                 [-> Cont [-> ExpVal FinalAnswer]]))
           cont))]
        [(trace-proc-exp vars body)
         (apply-cont cont (proc-val (trace-procedure vars body env)))]
        [(proc-exp vars body)
         (apply-cont cont (proc-val (procedure vars body env)))]
        [(call-exp rator rands)
         (value-of/k
          rator env
          (cons
           (frame
            'call-rator-frame
            (ann (λ (cont)
                   (λ (op)
                     (unless (or (proc? op) (primitive-proc? op))
                       (raise-argument-error 'value-of/k "operator?" op))

                     (if (var-exp? rands)
                         (value-of/k
                          rands env
                          (cons
                           (frame
                            'call-rator-frame
                            (ann (λ (cont)
                                   (λ (args)
                                     (cond [(proc? op)
                                            (apply-procedure/k op (expval->list args) cont)]
                                           [(primitive-proc? op)
                                            (apply-cont cont (apply (primitive-proc-λ op) (expval->list args)))])))
                                 [-> Cont [-> ExpVal FinalAnswer]]))
                           cont))
                         (let loop : FinalAnswer
                              ([rands rands] [args : (Listof DenVal) '()])
                           (if (null? rands)
                               (cond [(proc? op)
                                      (apply-procedure/k op (reverse args) cont)]
                                     [(primitive-proc? op)
                                      (apply-cont cont (apply (primitive-proc-λ op) (reverse args)))])
                               (value-of/k
                                (car rands) env
                                (cons
                                 (frame
                                  'call-rator-frame
                                  (ann (λ (cont)
                                         (λ (arg)
                                           (loop (cdr rands)
                                                 (cons (expval->denval arg) args))))
                                       [-> Cont [-> ExpVal FinalAnswer]]))
                                 cont)))))))
                 [-> Cont [-> ExpVal FinalAnswer]]))
           cont))])))

  )
