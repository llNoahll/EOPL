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
         "exp-sig.rkt")

(provide exp@)


(define-unit exp@
  (import ref^ cont^ thd^ sche^ mut^ values^ env^ proc^)
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
        [(symbol-exp sym)  (apply-cont cont (symbol-val sym))]
        [(real-exp   num)  (apply-cont cont (num-val    num))]
        [(bool-exp   bool) (apply-cont cont (bool-val   bool))]
        [(char-exp   char) (apply-cont cont (char-val   char))]
        [(string-exp str)  (apply-cont cont (string-val str))]

        [(var-exp    var)  (apply-cont cont (apply-env env var))]

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

        [(letrec-exp vars exps body)
         (cond [(or (null? exps) (null? vars))
                (value-of/k body env cont)]
               [else
                (define new-env
                  (extend-env+
                   (map (ann (λ (var) (cons var undefined))
                             [-> Symbol (Pair Symbol Undefined)])
                        vars)
                   env))
                (value-of/k
                 (car exps) new-env
                 (append
                  (for/list : Cont
                            ([var (in-list vars)]
                             [exp (in-list (append (cdr exps) (list body)))])
                    (frame
                     'letrec-frame
                     (ann (λ (cont)
                            (λ (val)
                              (set-binding! new-env var (expval->denval val))
                              (value-of/k exp new-env cont)))
                          [-> Cont [-> ExpVal FinalAnswer]])))
                  cont))])]

        [(spawn-exp exp)
         (value-of/k
          exp env
          (cons
           (frame
            'spawn-frame
            (ann (λ (cont)
                   (λ (op)
                     (: spawn-tid Natural)
                     (define spawn-tid (get-nid))

                     (: spawn-thk [-> FinalAnswer])
                     (define spawn-thk
                       (cond [(proc? op)
                              (λ ()
                                (apply-procedure/k
                                 (if (thread-share-memory?)
                                     op
                                     (proc (proc-vars op)
                                           (proc-body op)
                                           (copy-env (proc-saved-env op))))
                                 (list (num-val spawn-tid))
                                 (end-subthread-cont)))]
                             [(primitive-proc? op)
                              (λ ()
                                (apply-cont (end-subthread-cont)
                                            (apply-cont cont ((primitive-proc-λ op) (num-val spawn-tid)))))]
                             [else (raise-argument-error 'value-of/k "operator?" op)]))

                     (place-on-ready-queue! spawn-thk (get-tid) spawn-tid (box (empty-queue)))
                     (apply-cont cont (num-val spawn-tid))))
                 [-> Cont [-> ExpVal FinalAnswer]]))
           cont))]

        [(mutex-exp exp)
         (value-of/k
          exp env
          (cons
           (frame
            'mutex-frame
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
          (cons
           (frame
            'wait-frame
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
          (cons
           (frame
            'signal-frame
            (ann (λ (cont)
                   (λ (mut)
                     (signal-mutex
                      (expval->mutex mut)
                      (λ () (apply-cont cont (void))))))
                 [-> Cont [-> ExpVal FinalAnswer]]))
           cont))]
        [(yield-exp)
         (place-on-ready-queue! (λ () (apply-cont cont (num-val (get-tid)))))
         (run-next-thread)]
        [(kill-exp exp)
         (value-of/k
          exp env
          (cons
           (frame
            'kill-frame
            (ann (λ (cont)
                   (λ (tid)
                     (define res (kill-thread! (assert tid natural?)))
                     (if (boolean? res)
                         (apply-cont cont (bool-val res))
                         (run-next-thread))))
                 [-> Cont [-> ExpVal FinalAnswer]]))
           cont))]

        [(send-exp tid-exp value-exp)
         (value-of/k
          tid-exp env
          (cons
           (frame
            'send-frame
            (ann (λ (cont)
                   (λ (tid)
                     (if (has-thread? (assert tid natural?))
                         (value-of/k
                          value-exp env
                          (cons
                           (frame
                            'send-frame
                            (ann (λ (cont)
                                   (λ (val)
                                     (define th (get-thread (assert tid natural?)))
                                     (let ([mail (if (thd? th) (thd-mail th) (get-mail))])
                                       (set-box! mail (enqueue (unbox mail) (expval->denval val)))
                                       (apply-cont cont (void)))))
                                 [-> Cont [-> ExpVal FinalAnswer]]))
                           cont))
                         (apply-cont cont (num-val (get-tid))))))
                 [-> Cont [-> ExpVal FinalAnswer]]))
           cont))]
        [(receive-exp)
         (cond [(empty-queue? (unbox (get-mail)))
                (place-on-ready-queue! (λ () (value-of/k (receive-exp) env cont)))
                (run-next-thread)]
               [else (value-of/k (try-receive-exp) env cont)])]
        [(try-receive-exp)
         (define mail (get-mail))
         (define value-queue (unbox mail))
         (apply-cont
          cont
          (if (empty-queue? value-queue)
              (bool-val #f)
              (dequeue value-queue
                       (ann (λ (1st-value other-values)
                              (set-box! mail other-values)
                              1st-value)
                            [-> DenVal (Queueof DenVal) DenVal]))))]

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
