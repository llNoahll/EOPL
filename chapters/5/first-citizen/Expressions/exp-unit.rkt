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


  (: value-of/k [-> Exp Env Cont* FinalAnswer])
  (define value-of/k
    (λ (exp env saved-cont)
      (match exp
        [(assign-exp var exp)
         (value-of/k exp env
                     (cont 'assign-cont
                           (λ () saved-cont)
                           (ann (λ (val)
                                  (apply-cont saved-cont
                                              (set-binding! env var (expval->denval val))))
                                [-> ExpVal FinalAnswer])))]
        [(symbol-exp sym) (apply-cont saved-cont (symbol-val sym))]
        [(const-exp num)  (apply-cont saved-cont (num-val num))]
        [(bool-exp bool)  (apply-cont saved-cont (bool-val bool))]
        [(char-exp char)  (apply-cont saved-cont (char-val char))]
        [(string-exp str) (apply-cont saved-cont (string-val str))]
        [(var-exp var)    (apply-cont saved-cont (apply-env env var))]

        [(begin-exp exps)
         (let loop : FinalAnswer ([exps exps])
           (define next (cdr exps))
           (value-of/k (car exps) env
                       (if (null? next)
                           saved-cont
                           (cont 'begin-cont
                                 (λ () saved-cont)
                                 (ann (λ (val) (loop next))
                                      [-> ExpVal FinalAnswer])))))]

        [(if-exp pred-exp true-exp false-exp)
         (value-of/k pred-exp env
                     (cont 'if-cont
                           (λ () saved-cont)
                           (ann (λ (pred-val)
                                  (value-of/k (if (expval->bool pred-val)
                                                  true-exp
                                                  false-exp)
                                              env saved-cont))
                                [-> ExpVal FinalAnswer])))]
        [(cond-exp branches)
         (let loop : FinalAnswer ([branches branches])
           (cond [(null? branches) (apply-cont saved-cont (void))]
                 [else
                  (define branch (car branches))
                  (value-of/k (car branch) env
                              (cont 'cond-cont
                                    (λ () saved-cont)
                                    (ann (λ (pred-val)
                                           (if (expval->bool pred-val)
                                               (value-of/k (cadr branch) env saved-cont)
                                               (loop (cdr branches))))
                                         [-> ExpVal FinalAnswer])))]))]

        [(let-exp vars exps body)
         (let loop : FinalAnswer
              ([exps exps]
               [vals : (Listof DenVal) '()])
           (if (null? exps)
               (value-of/k body
                           (extend-env* vars (reverse vals) env)
                           saved-cont)
               (value-of/k (car exps) env
                           (cont 'let-cont
                                 (λ () saved-cont)
                                 (ann (λ (val)
                                        (loop (cdr exps) (cons (expval->denval val) vals)))
                                      [-> ExpVal FinalAnswer])))))]
        [(letrec-exp vars exps body)
         (define new-env
           (extend-env+ (map (ann (λ (var) (cons var undefined))
                                  [-> Symbol (Pair Symbol Undefined)])
                             vars)
                        env))

         (let loop : FinalAnswer ([exps exps] [vars vars])
           (if (null? exps)
               (value-of/k body new-env saved-cont)
               (value-of/k (car exps) new-env
                           (cont 'letrec-cont
                                 (λ () saved-cont)
                                 (ann (λ (val)
                                        (set-binding! new-env (car vars) (expval->denval val))
                                        (loop (cdr exps) (cdr vars)))
                                      [-> ExpVal FinalAnswer])))))]

        [(handlers-exp catch-preds catch-handlers body)
         (let loop : FinalAnswer
              ([pred-exps    catch-preds]
               [handler-exps catch-handlers]
               [pred-vals    : (Listof Proc) '()]
               [handler-vals : (Listof Proc) '()])
           (if (or (null? pred-exps) (null? handler-exps))
               (value-of/k body env
                           (handlers-cont 'handlers-cont
                                          (λ () saved-cont)
                                          (cont-func saved-cont)
                                          (reverse pred-vals)
                                          (reverse handler-vals)))
               (value-of/k
                (car pred-exps) env
                (cont 'handlers-cont
                      (λ () saved-cont)
                      (ann (λ (val)
                             (define pred-val (expval->proc val))
                             (value-of/k
                              (car handler-exps) env
                              (cont 'handlers-cont
                                    (λ () saved-cont)
                                    (ann (λ (val)
                                           (define handler-val (expval->proc val))
                                           (loop (cdr pred-exps)
                                                 (cdr handler-exps)
                                                 (cons pred-val pred-vals)
                                                 (cons handler-val handler-vals)))
                                         [-> ExpVal FinalAnswer]))))
                           [-> ExpVal FinalAnswer])))))]
        [(raise-exp exp)
         (value-of/k
          exp env
          (cont 'raise-cont
                (λ () saved-cont)
                (ann (λ (val)
                       (: args (Listof DenVal))
                       (define args (list (expval->denval val)))
                       (let loop : FinalAnswer ([saved-cont saved-cont])
                         (cond [(handlers-cont? saved-cont)
                                (let check : FinalAnswer
                                     ([preds    (handlers-cont-preds    saved-cont)]
                                      [handlers (handlers-cont-handlers saved-cont)])
                                  (if (or (null? preds) (null? handlers))
                                      (loop ((cont-saved-cont saved-cont)))
                                      (apply-procedure/k
                                       (car preds) args
                                       (cont 'raise-cont
                                             (λ () saved-cont)
                                             (ann (λ (val)
                                                    (if (expval->bool val)
                                                        (apply-procedure/k (car handlers) args saved-cont)
                                                        (check (cdr preds) (cdr handlers))))
                                                  [-> ExpVal FinalAnswer])))))]
                               [(end-cont? saved-cont) (error "uncaught exception: " val)]
                               [else (loop ((cont-saved-cont saved-cont)))])))
                     [-> ExpVal FinalAnswer])))]

        [(primitive-proc-exp op exps)
         (let loop : FinalAnswer
              ([exps exps]
               [vals : (Listof DenVal) '()])
           (if (null? exps)
               (apply-cont saved-cont
                           (apply (hash-ref primitive-proc-table op)
                                  (reverse vals)))
               (value-of/k (car exps) env
                           (cont 'primitive-proc-cont
                                 (λ () saved-cont)
                                 (ann (λ (val)
                                        (loop (cdr exps) (cons (expval->denval val) vals)))
                                      [-> ExpVal FinalAnswer])))))]
        [(trace-proc-exp vars body)
         (apply-cont saved-cont (proc-val (trace-procedure vars body env)))]
        [(proc-exp vars body)
         (apply-cont saved-cont (proc-val (procedure vars body env)))]
        [(call-exp rator rands)
         (value-of/k
          rator env
          (cont 'call-rator-cont
                (λ () saved-cont)
                (ann (λ (val)
                       (define proc (expval->proc val))
                       (if (var-exp? rands)
                           (value-of/k
                            rands env
                            (cont 'call-rator-cont
                                  (λ () saved-cont)
                                  (ann (λ (args)
                                         (apply-procedure/k proc (cast args (Listof DenVal)) saved-cont))
                                       [-> ExpVal FinalAnswer])))
                           (let loop : FinalAnswer
                                ([rands rands] [args : (Listof DenVal) '()])
                             (if (null? rands)
                                 (apply-procedure/k proc (reverse args) saved-cont)
                                 (value-of/k
                                  (car rands) env
                                  (cont 'call-rator-cont
                                        (λ () saved-cont)
                                        (ann (λ (arg)
                                               (loop (cdr rands)
                                                     (cons (expval->denval arg) args)))
                                             [-> ExpVal FinalAnswer])))))))
                     [-> ExpVal FinalAnswer])))])))

  )
