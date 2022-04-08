#lang typed/racket

(require "../types/types.rkt"
         "../Reference/ref-sig.rkt"
         "../Continuation/cont-sig.rkt"
         "../Thread/thd-sig.rkt"
         "../Scheduler/sche-sig.rkt"
         "../Mutex/mut-sig.rkt"
         "../ExpValues/values-sig.rkt"
         "../Environment/env-sig.rkt"
         "../TypeEnvironment/tenv-sig.rkt"
         "../Procedure/proc-sig.rkt"
         "exp-sig.rkt")

(provide exp@)


(define-unit exp@
  (import ref^ cont^ thd^ sche^ mut^ values^ env^ tenv^ proc^)
  (export exp^)


  (: value-of/k [-> Exp Env Cont FinalAnswer])
  (define value-of/k
    (λ (exp env cont)
      (match exp
        [(ann-exp exp type) (value-of/k exp env cont)]

        [(assign-exp var exp)
         (value-of/k
          exp env
          (cons
           (frame
            'assign-frame
            (inherit-handlers-cont cont)
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
          (cons
           (frame
            'if-frame
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
          (cons
           (frame
            'raise-frame
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
                                (apply-procedure/k
                                 (if (thread-share-memory?)
                                     op
                                     (proc (proc-vars op)
                                           (proc-body op)
                                           (copy-env (proc-saved-env op))))
                                 (list (num-val spawn-tid))
                                 (end-subthread-cont)))]
                             [(cont? op)
                              (λ ()
                                (apply-cont (end-subthread-cont)
                                            (apply-cont op (num-val spawn-tid))))]
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
          (cons
           (frame
            'wait-frame
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
          (cons
           (frame
            'signal-frame
            (inherit-handlers-cont cont)
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
            (inherit-handlers-cont cont)
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
            (inherit-handlers-cont cont)
            (ann (λ (cont)
                   (λ (tid)
                     (if (has-thread? (assert tid natural?))
                         (value-of/k
                          value-exp env
                          (cons
                           (frame
                            'send-frame
                            (inherit-handlers-cont cont)
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
            (inherit-handlers-cont cont)
            (ann (λ (cont)
                   (λ (op)
                     (unless (or (proc? op) (cont? op) (primitive-proc? op))
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
                                            (apply-cont op (car (expval->list args)))]
                                           [(primitive-proc? op)
                                            (apply-cont cont (apply (primitive-proc-λ op) (expval->list args)))])))
                                 [-> Cont [-> ExpVal FinalAnswer]]))
                           cont))
                         (let loop : FinalAnswer
                              ([rands rands] [args : (Listof DenVal) '()])
                           (if (null? rands)
                               (cond [(proc? op)
                                      (apply-procedure/k op (reverse args) cont)]
                                     [(cont? op)
                                      (apply-cont op (car (last-pair args)))]
                                     [(primitive-proc? op)
                                      (apply-cont cont (apply (primitive-proc-λ op) (reverse args)))])
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
           cont))])))


  (: type-of [-> Exp TEnv (Option Type) [#:safe? Boolean] Type])
  (define type-of
    (λ (exp tenv t0 #:safe? [safe? #t])
      (: check [-> Type Type])
      (define check
        (λ (t1)
          (if (or (false? t0) (<=: t1 t0))
              t1
              (raise-arguments-error 'type-of "type mismatch"
                                     "expected" t0
                                     "given"    t1
                                     "in"       exp))))

      (match exp
        [(ann-exp exp type)
         (begin0 (check type)
           (when safe? (type-of exp tenv type)))]

        [(assign-exp var exp)
         (begin0 (check 'Void)
           (when safe? (type-of exp tenv (apply-tenv tenv var))))]

        [(symbol-exp sym)  (check 'Symbol)]
        [(real-exp   num)  (check 'Real)]
        [(bool-exp   #t)   (check 'True)]
        [(bool-exp   #f)   (check 'False)]
        [(char-exp   char) (check 'Char)]
        [(string-exp str)  (check 'String)]

        [(var-exp var)
         (define t1 (apply-tenv tenv var))
         (if safe?
             (check (assert t1))
             (or (and t1 (check t1)) t0 'Any))]

        [(begin-exp exps)
         (if safe?
             (let loop ([exp (car exps)] [next (cdr exps)])
               (cond [(null? next) (type-of exp tenv t0)]
                     [else
                      (type-of exp tenv 'Void)
                      (loop (car next) (cdr next))]))
             (type-of (car (last-pair exps))
                      tenv t0 #:safe? safe?))]

        [(if-exp pred-exp true-exp false-exp)
         (define tp (type-of pred-exp  tenv 'Boolean))
         (define tt (type-of true-exp  tenv t0))
         (define tf (type-of false-exp tenv t0))
         (case tp
           [(True)    tt]
           [(False)   tf]
           [(Boolean) (check (type-union tt tf))]
           [else
            (raise-arguments-error 'type-of "type mismatch"
                                   "expected" 'Boolean
                                   "given"    tp
                                   "in"       pred-exp)])]

        [(letrec-exp vars exps body)
         (cond [(or (null? exps) (null? vars))
                (type-of body tenv t0)]
               [else
                (define tenv0
                  (for/fold ([res : TEnv tenv])
                            ([var (in-list vars)]
                             [exp (in-list exps)])
                    (extend-tenv var #f res)))

                (define tenv1
                  (for/fold ([res : TEnv tenv0])
                            ([var (in-list vars)]
                             [exp (in-list exps)])
                    (define t1 (type-of exp res #f #:safe? #f))
                    (extend-tenv var t1 res)))

                (for ([var (in-list vars)]
                      [exp (in-list exps)])
                  (type-of exp tenv1 (apply-tenv tenv1 var)))

                (type-of body tenv1 t0)])]

        [(let/cc-exp cc-var body)
         (define tenv0 (extend-tenv cc-var `[-> ,(assert t0) Nothing] tenv))
         (type-of body tenv0 t0)]

        [(handlers-exp catch-preds catch-handlers body)
         (begin0 (type-of body tenv t0)
           (for ([catch-pred    (in-list catch-preds)]
                 [catch-handler (in-list catch-handlers)])
             (assert (type-of catch-pred tenv #f) λ-type?)
             (check (λ-return-type (assert (type-of catch-handler tenv #f) λ-type?)))))]
        [(raise-exp exp)  (begin0 (check 'Nothing) (when safe? (type-of exp tenv #f)))]
        [(spawn-exp exp)  (begin0 (check 'Natural) (when safe? (type-of exp tenv #f)))]
        [(mutex-exp exp)  (begin0 (check 'Mutex)   (when safe? (type-of exp tenv 'Natural)))]
        [(wait-exp exp)   (begin0 (check 'Void)    (when safe? (type-of exp tenv 'Mutex)))]
        [(signal-exp exp) (begin0 (check 'Void)    (when safe? (type-of exp tenv 'Mutex)))]
        [(yield-exp)      (check 'Natural)]
        [(kill-exp exp)   (begin0 (check 'Boolean) (when safe? (type-of exp tenv 'Natural)))]

        [(send-exp tid-exp value-exp)
         (begin0 (check 'Void)
           (type-of tid-exp tenv 'Natural)
           (type-of value-exp tenv #f))]
        [(receive-exp)     (assert t0)]
        [(try-receive-exp) (assert t0)]

        #;[(trace-proc-exp vars body) (type-of (proc-exp vars body) tenv)]
        [(proc-exp vars body) (assert t0)]
        [(call-exp rator rands)
         (match rator
           [(proc-exp vars body)
            (: ts (Listof Type))
            (define ts
              (if (list? rands)
                  (for/list : (Listof Type)
                            ([rand (in-list rands)])
                    (type-of rand tenv #f))
                  (match (type-of rands tenv #f)
                    [`(List ,(? type? #{ts : (Listof Type)}) ...) ts])))

            (type-of body
                     (if (list? vars)
                         (extend-tenv* vars ts tenv)
                         (extend-tenv  vars `(List ,@ts) tenv))
                     t0)]
           [_
            (: ts (Listof Type))
            (: t1 Type)
            (define-values (ts t1)
              (match (type-of rator tenv #f)
                [`[-> ,ts ... ,t* * ,t1]
                 #:when (and (types? ts) (type? t*) (type? t1))
                 (values
                  (if (list? rands)
                      (append ts (build-list (- (length rands) (length ts)) (const t*)))
                      (match (type-of rands tenv #f)
                        [`(List ,ts0 ...)
                         #:when (types? ts0)
                         (begin0 ts0
                           (let loop ([ts0 ts0] [ts ts])
                             (cond
                               [(null? ts0) (assert (null? ts))]
                               [(null? ts)
                                (define t0 (car ts0))
                                (if (<=: t0 t*)
                                    (loop (cdr ts0) ts)
                                    (raise-arguments-error 'type-of "type mismatch"
                                                           "expected" t*
                                                           "given"    t0
                                                           "in"       exp))]
                               [else
                                (define t0 (car ts0))
                                (define t  (car ts))
                                (if (<=: t0 t)
                                    (loop (cdr ts0) (cdr ts))
                                    (raise-arguments-error 'type-of "type mismatch"
                                                           "expected" t
                                                           "given"    t0
                                                           "in"       exp))])))]))
                  (check t1))]
                [`[-> ,ts ... ,t1]
                 #:when (and (types? ts) (type? t1))
                 (when (list? rands)
                   (assert (= (length rands) (length ts))))
                 (values ts (check t1))]))
            (if (list? rands)
                (for ([t    (in-list ts)]
                      [rand (in-list rands)])
                  (type-of rand tenv t))
                (type-of rands tenv `(List ,@ts)))
            t1])])))

  )
