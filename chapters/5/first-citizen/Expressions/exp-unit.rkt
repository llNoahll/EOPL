#lang typed/racket

(require "../types/types.rkt"
         "../Reference/ref-sig.rkt"
         "../Continuation/cont-sig.rkt"
         "../Thread/thd-sig.rkt"
         "../Scheduler/sche-sig.rkt"
         "../Mutex/mut-sig.rkt"
         "../ExpValues/values-sig.rkt"
         "../Environment/env-sig.rkt"
         "../Procedure/proc-sig.rkt"
         "../PrimitiveProc/primitive-proc-sig.rkt"
         "exp-sig.rkt")

(provide exp@)


(define-unit exp@
  (import ref^ cont^ thd^ sche^ mut^ values^ env^ proc^ primitive-proc^)
  (export exp^)


  (: value-of/k [-> Exp Env Cont FinalAnswer])
  (define value-of/k
    (let ()
      (: return [-> Cont [-> ExpVal FinalAnswer]])
      (define return
        (λ (cont)
          (λ (val)
            (apply-cont cont val))))

      (λ (exp env cont)
        (match exp
          [(assign-exp var exp)
           (value-of/k
            exp env
            (cons (frame 'assign-frame
                          (inherit-handlers-cont cont)
                          (ann (λ (cont)
                                 (λ (val)
                                   (apply-cont cont (set-binding! env var (expval->denval val)))))
                               [-> Cont [-> ExpVal FinalAnswer]]))
                  cont))]
          [(symbol-exp sym) (apply-cont cont (symbol-val sym))]
          [(const-exp num)  (apply-cont cont (num-val num))]
          [(bool-exp bool)  (apply-cont cont (bool-val bool))]
          [(char-exp char)  (apply-cont cont (char-val char))]
          [(string-exp str) (apply-cont cont (string-val str))]
          [(var-exp var)    (apply-cont cont (apply-env env var))]

          [(begin-exp exps)
           (value-of/k
            (car exps) env
            (cons (frame 'begin-frame
                          (inherit-handlers-cont cont)
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
            (cons (frame 'if-frame
                          (inherit-handlers-cont cont)
                          (ann (λ (cont)
                                 (λ (pred-val)
                                   (value-of/k
                                    (if (expval->bool pred-val)
                                        true-exp
                                        false-exp)
                                    env cont)))
                               [-> Cont [-> ExpVal FinalAnswer]]))
                  cont))]
          [(cond-exp branches)
           (define branch (car branches))
           (value-of/k
            (car branch) env
            (cons (frame 'cond-frame
                         (inherit-handlers-cont cont)
                         (ann (λ (cont)
                                (λ (pred-val)
                                  (if (expval->bool pred-val)
                                      (value-of/k (cadr branch) env cont)
                                      (let ([next (cdr branches)])
                                        (if (null? next)
                                            (apply-cont cont (void))
                                            (value-of/k (cond-exp next) env cont))))))
                              [-> Cont [-> ExpVal FinalAnswer]]))
                  cont))]

          [(let-exp vars exps body)
           (value-of/k (if (or (null? exps) (null? vars))
                           body
                           (call-exp (proc-exp vars body) exps))
                       env cont)]
          [(letrec-exp vars exps body)
           (cond [(or (null? exps) (null? vars))
                  (value-of/k body env cont)]
                 [else
                  (define new-env
                    (extend-env+ (map (ann (λ (var) (cons var undefined))
                                           [-> Symbol (Pair Symbol Undefined)])
                                      vars)
                                 env))
                  (value-of/k
                   (car exps) new-env
                   (append
                    (for/list : Cont
                              ([var (in-list vars)]
                               [exp (in-list (append (cdr exps) (list body)))])
                      (frame 'letrec-frame
                              (inherit-handlers-cont cont)
                              (ann (λ (cont)
                                     (λ (val)
                                       (set-binding! new-env var (expval->denval val))
                                       (value-of/k exp new-env cont)))
                                   [-> Cont [-> ExpVal FinalAnswer]])))
                    cont))])]

          [(let/cc-exp cc-var body) (value-of/k body (extend-env cc-var cont env) cont)]

          [(handlers-exp catch-preds catch-handlers body)
           (cond
             [(or (null? catch-preds) (null? catch-handlers))
              (value-of/k body env cont)]
             [(= 1 (length catch-preds) (length catch-handlers))
              (value-of/k
               (car catch-preds) env
               (cons
                (frame
                 'handlers-frame
                 (inherit-handlers-cont cont)
                 (ann (λ (cont)
                        (λ (val)
                          (define pred-val (expval->proc val))
                          (value-of/k
                           (car catch-handlers) env
                           (cons
                            (frame
                             'handlers-frame
                             (inherit-handlers-cont cont)
                             (ann (λ (cont)
                                    (λ (val)
                                      (define handler-val (expval->proc val))
                                      (define frame (car cont))
                                      (define cont* (cdr cont))
                                      (value-of/k
                                       body env
                                       (cons
                                        (if (handlers-frame? frame)
                                            (handlers-frame
                                             'handlers-frame
                                             (inherit-handlers-cont cont*)
                                             (frame-func frame)
                                             (cons    pred-val (handlers-frame-preds    frame))
                                             (cons handler-val (handlers-frame-handlers frame)))
                                            (handlers-frame
                                             'handlers-frame
                                             (inherit-handlers-cont cont)
                                             (frame-func frame)
                                             (list    pred-val)
                                             (list handler-val)))
                                        cont*))))
                                  [-> Cont [-> ExpVal FinalAnswer]]))
                            cont))))
                      [-> Cont [-> ExpVal FinalAnswer]]))
                cont))]
             [else
              (value-of/k
               (handlers-exp (cdr catch-preds)
                             (cdr catch-handlers)
                             (handlers-exp (list (car catch-preds))
                                           (list (car catch-handlers))
                                           body))
               env cont)])]
          [(raise-exp exp)
           (value-of/k
            exp env
            (cons (frame 'raise-frame
                          (inherit-handlers-cont cont)
                          (ann (λ (cont)
                                 (λ (val)
                                   (apply-handler cont (expval->denval val))))
                               [-> Cont [-> ExpVal FinalAnswer]]))
                  cont))]

          [(spawn-exp exp)
           (value-of/k
            exp env
            (cons
             (frame
              'spawn-frame
              (inherit-handlers-cont cont)
              (ann (λ (cont)
                     (λ (op)
                       (: spawn-tid Natural)
                       (define spawn-tid (get-nid))

                       (: spawn-thk [-> FinalAnswer])
                       (define spawn-thk
                         (cond [(proc? op)
                                (λ ()
                                  (apply-procedure/k op
                                                     (list (num-val spawn-tid))
                                                     (end-subthread-cont)))]
                               [(cont? op)
                                (λ ()
                                  (apply-cont (end-subthread-cont)
                                              (apply-cont op (num-val spawn-tid))))]
                               [else (raise-argument-error 'value-of/k "operator?" op)]))

                       (place-on-ready-queue! spawn-thk (get-tid) spawn-tid)
                       (apply-cont cont (num-val spawn-tid))))
                   [-> Cont [-> ExpVal FinalAnswer]]))
             cont))]

          [(mutex-exp exp)
           (value-of/k
            exp env
            (cons (frame 'mutex-frame
                          (inherit-handlers-cont cont)
                          (ann (λ (cont)
                                 (λ (keys)
                                   (apply-cont cont
                                               (mutex-val
                                                (mutex (assert keys natural?)
                                                       (empty-queue))))))
                               [-> Cont [-> ExpVal FinalAnswer]]))
                  cont))]
          [(wait-exp exp)
           (value-of/k
            exp env
            (cons (frame 'wait-frame
                          (inherit-handlers-cont cont)
                          (ann (λ (cont)
                                 (λ (mut)
                                   (wait-for-mutex
                                    (expval->mutex mut)
                                    (λ () (apply-cont cont (void))))))
                               [-> Cont [-> ExpVal FinalAnswer]]))
                  cont))]
          [(signal-exp exp)
           (value-of/k
            exp env
            (cons (frame 'signal-frame
                         (inherit-handlers-cont cont)
                         (ann (λ (cont)
                                (λ (mut)
                                  (signal-mutex
                                   (expval->mutex mut)
                                   (λ () (apply-cont cont (void))))))
                              [-> Cont [-> ExpVal FinalAnswer]]))
                  cont))]
          [(yield-exp)
           (place-on-ready-queue! (λ () (apply-cont cont (num-val 99))))
           (run-next-thread)]

          [(primitive-proc-exp op exps)
           (let loop : FinalAnswer
                ([exps exps]
                 [vals : (Listof DenVal) '()])
             (if (null? exps)
                 (apply-cont cont
                             (apply (hash-ref primitive-proc-table op)
                                    (reverse vals)))
                 (value-of/k (car exps) env
                             (cons
                              (frame 'primitive-proc-frame
                                     (inherit-handlers-cont cont)
                                     (ann (λ (cont)
                                            (λ (val)
                                              (loop (cdr exps)
                                                    (cons (expval->denval val) vals))))
                                          [-> Cont [-> ExpVal FinalAnswer]]))
                              cont))))]
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
              (inherit-handlers-cont cont)
              (ann (λ (cont)
                     (λ (op)
                       (unless (or (proc? op) (cont? op))
                         (raise-argument-error 'value-of/k "operator?" op))

                       (if (var-exp? rands)
                           (value-of/k
                            rands env
                            (cons
                             (frame
                              'call-rator-frame
                              (inherit-handlers-cont cont)
                              (ann (λ (cont)
                                     (λ (args)
                                       (cond [(proc? op)
                                              (apply-procedure/k op (expval->list args) cont)]
                                             [(cont? op)
                                              (apply-cont op (car (expval->list args)))])))
                                   [-> Cont [-> ExpVal FinalAnswer]]))
                             cont))
                           (let loop : FinalAnswer
                                ([rands rands] [args : (Listof DenVal) '()])
                             (if (null? rands)
                                 (cond [(proc? op)
                                        (apply-procedure/k op (reverse args) cont)]
                                       [(cont? op)
                                        (apply-cont op (car (last-pair args)))])
                                 (value-of/k
                                  (car rands) env
                                  (cons
                                   (frame
                                    'call-rator-frame
                                    (inherit-handlers-cont cont)
                                    (ann (λ (cont)
                                           (λ (arg)
                                             (loop (cdr rands)
                                                   (cons (expval->denval arg) args))))
                                         [-> Cont [-> ExpVal FinalAnswer]]))
                                   cont)))))))
                   [-> Cont [-> ExpVal FinalAnswer]]))
             cont))]))))

  )
