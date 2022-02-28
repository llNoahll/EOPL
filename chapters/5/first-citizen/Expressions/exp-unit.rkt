#lang typed/racket

(require "../types/types.rkt"
         "../Reference/ref-sig.rkt"
         "../Continuation/cont-sig.rkt"
         "../Scheduler/sche-sig.rkt"
         "../Mutex/mut-sig.rkt"
         "../ExpValues/values-sig.rkt"
         "../Environment/env-sig.rkt"
         "../Procedure/proc-sig.rkt"
         "../PrimitiveProc/primitive-proc-sig.rkt"
         "exp-sig.rkt")

(provide exp@)


(define-unit exp@
  (import ref^ cont^ sche^ mut^ values^ env^ proc^ primitive-proc^)
  (export exp^)


  (: value-of/k [-> Exp Env Cont* FinalAnswer])
  (define value-of/k
    (λ (exp env cont*)
      (match exp
        [(assign-exp var exp)
         (value-of/k exp env
                     (cont 'assign-cont
                           (inherit-handlers-cont* cont*)
                           (ann (λ (val) (apply-cont cont* (set-binding! env var (expval->denval val))))
                                [-> ExpVal FinalAnswer])))]
        [(symbol-exp sym) (apply-cont cont* (symbol-val sym))]
        [(const-exp num)  (apply-cont cont* (num-val num))]
        [(bool-exp bool)  (apply-cont cont* (bool-val bool))]
        [(char-exp char)  (apply-cont cont* (char-val char))]
        [(string-exp str) (apply-cont cont* (string-val str))]
        [(var-exp var)    (apply-cont cont* (apply-env env var))]

        [(begin-exp exps)
         (let loop : FinalAnswer ([exps exps])
           (define next (cdr exps))
           (value-of/k (car exps) env
                       (if (null? next)
                           cont*
                           (cont 'begin-cont
                                 (inherit-handlers-cont* cont*)
                                 (ann (λ (val) (loop next))
                                      [-> ExpVal FinalAnswer])))))]

        [(if-exp pred-exp true-exp false-exp)
         (value-of/k pred-exp env
                     (cont 'if-cont
                           (inherit-handlers-cont* cont*)
                           (ann (λ (pred-val)
                                  (value-of/k (if (expval->bool pred-val)
                                                  true-exp
                                                  false-exp)
                                              env cont*))
                                [-> ExpVal FinalAnswer])))]
        [(cond-exp branches)
         (let loop : FinalAnswer ([branches branches])
           (cond [(null? branches) (apply-cont cont* (void))]
                 [else
                  (define branch (car branches))
                  (value-of/k (car branch) env
                              (cont 'cond-cont
                                    (inherit-handlers-cont* cont*)
                                    (ann (λ (pred-val)
                                           (if (expval->bool pred-val)
                                               (value-of/k (cadr branch) env cont*)
                                               (loop (cdr branches))))
                                         [-> ExpVal FinalAnswer])))]))]

        [(let-exp vars exps body)
         (let loop : FinalAnswer
              ([exps exps]
               [vals : (Listof DenVal) '()])
           (if (null? exps)
               (value-of/k body
                           (extend-env* vars (reverse vals) env)
                           cont*)
               (value-of/k (car exps) env
                           (cont 'let-cont
                                 (inherit-handlers-cont* cont*)
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
               (value-of/k body new-env cont*)
               (value-of/k (car exps) new-env
                           (cont 'letrec-cont
                                 (inherit-handlers-cont* cont*)
                                 (ann (λ (val)
                                        (set-binding! new-env (car vars) (expval->denval val))
                                        (loop (cdr exps) (cdr vars)))
                                      [-> ExpVal FinalAnswer])))))]

        [(let/cc-exp cc-var body) (value-of/k body (extend-env cc-var cont* env) cont*)]

        [(handlers-exp catch-preds catch-handlers body)
         (let loop : FinalAnswer
              ([pred-exps    catch-preds]
               [handler-exps catch-handlers]
               [pred-vals    : (Listof Proc) '()]
               [handler-vals : (Listof Proc) '()])
           (if (or (null? pred-exps) (null? handler-exps))
               (value-of/k body env
                           (handlers-cont 'handlers-cont
                                          (inherit-handlers-cont* cont*)
                                          (cont-func cont*)
                                          (reverse pred-vals)
                                          (reverse handler-vals)))
               (value-of/k
                (car pred-exps) env
                (cont 'handlers-cont
                      (inherit-handlers-cont* cont*)
                      (ann (λ (val)
                             (define pred-val (expval->proc val))
                             (value-of/k
                              (car handler-exps) env
                              (cont 'handlers-cont
                                    (inherit-handlers-cont* cont*)
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
                (inherit-handlers-cont* cont*)
                (ann (λ (val) (apply-handler cont* (expval->denval val)))
                     [-> ExpVal FinalAnswer])))]

        [(spawn-exp exp)
         (value-of/k
          exp env
          (cont 'spawn-cont
                (inherit-handlers-cont* cont*)
                (ann (λ (caller)
                       (: spawn-thd Thd)
                       (define spawn-thd
                         (cond [(proc? caller)
                                (λ ()
                                  (apply-procedure/k caller
                                                     (list undefined)
                                                     (end-subthread-cont*)))]
                               [(cont? caller)
                                (λ ()
                                  (apply-cont (end-subthread-cont*)
                                              (apply-cont caller undefined)))]
                               [else (raise-argument-error 'value-of/k "caller?" caller)]))

                       (place-on-ready-queue! spawn-thd)
                       (apply-cont cont* (void)))
                     [-> ExpVal FinalAnswer])))]

        [(mutex-exp exp)
         (value-of/k
          exp env
          (cont 'mutex-cont
                (inherit-handlers-cont* cont*)
                (ann (λ (keys)
                       (apply-cont cont*
                                   (mutex-val (mutex (assert keys natural?) (empty-queue)))))
                     [-> ExpVal FinalAnswer])))]
        [(wait-exp exp)
         (value-of/k
          exp env
          (cont 'wait-cont
                (inherit-handlers-cont* cont*)
                (ann (λ (mut)
                       (wait-for-mutex
                        (expval->mutex mut)
                        (λ () (apply-cont cont* (void)))))
                     [-> ExpVal FinalAnswer])))]
        [(signal-exp exp)
         (value-of/k
          exp env
          (cont 'signal-cont
                (inherit-handlers-cont* cont*)
                (ann (λ (mut)
                       (signal-mutex
                        (expval->mutex mut)
                        (λ () (apply-cont cont* (void)))))
                     [-> ExpVal FinalAnswer])))]

        [(primitive-proc-exp op exps)
         (let loop : FinalAnswer
              ([exps exps]
               [vals : (Listof DenVal) '()])
           (if (null? exps)
               (apply-cont cont*
                           (apply (hash-ref primitive-proc-table op)
                                  (reverse vals)))
               (value-of/k (car exps) env
                           (cont 'primitive-proc-cont
                                 (inherit-handlers-cont* cont*)
                                 (ann (λ (val)
                                        (loop (cdr exps) (cons (expval->denval val) vals)))
                                      [-> ExpVal FinalAnswer])))))]
        [(trace-proc-exp vars body)
         (apply-cont cont* (proc-val (trace-procedure vars body env)))]
        [(proc-exp vars body)
         (apply-cont cont* (proc-val (procedure vars body env)))]
        [(call-exp rator rands)
         (value-of/k
          rator env
          (cont 'call-rator-cont
                (inherit-handlers-cont* cont*)
                (ann (λ (caller)
                       (unless (or (proc? caller) (cont? caller))
                         (raise-argument-error 'value-of/k "caller?" caller))

                       (if (var-exp? rands)
                           (value-of/k
                            rands env
                            (cont 'call-rator-cont
                                  (inherit-handlers-cont* cont*)
                                  (ann (λ (args)
                                         (cond [(proc? caller)
                                                (apply-procedure/k caller (cast args (Listof DenVal)) cont*)]
                                               [(cont? caller)
                                                (apply-cont caller (car (cast args (Listof DenVal))))]))
                                       [-> ExpVal FinalAnswer])))
                           (let loop : FinalAnswer
                                ([rands rands] [args : (Listof DenVal) '()])
                             (if (null? rands)
                                 (cond [(proc? caller)
                                        (apply-procedure/k caller (reverse args) cont*)]
                                       [(cont? caller)
                                        (apply-cont caller (car (last-pair args)))])
                                 (value-of/k
                                  (car rands) env
                                  (cont 'call-rator-cont
                                        (inherit-handlers-cont* cont*)
                                        (ann (λ (arg)
                                               (loop (cdr rands)
                                                     (cons (expval->denval arg) args)))
                                             [-> ExpVal FinalAnswer])))))))
                     [-> ExpVal FinalAnswer])))])))

  )
