#lang typed/racket

(require "../types/types.rkt"
         "../Parse/parse.rkt"
         "../Reference/ref-sig.rkt"
         "../Continuation/cont-sig.rkt"
         "../ExpValues/values-sig.rkt"
         "../Environment/env-sig.rkt"
         "../TypeEnvironment/tenv-sig.rkt"
         "../Procedure/proc-sig.rkt"
         "exp-sig.rkt")

(provide exp@)


(define-unit exp@
  (import ref^ cont^ values^ env^ tenv^ proc^)
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
        [(bytes-exp  bs)    (apply-cont cont (bytes-val  bs))]

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

  (: type-of [-> Exp TEnv (Option Type) Type])
  (define type-of
    (λ (exp tenv t0)
      (: check [-> Type Type])
      (define check
        (λ (t1)
          (if (or (false? t0) (<=: t1 t0))
              (match t1
                [`(Values ,t) t]
                [_ t1])
              (raise-arguments-error 'type-of   "type mismatch"
                                     "expected" t0
                                     "given"    t1
                                     "in"       exp))))

      (match exp
        [(ann-exp exp type) (begin0 (check type) (type-of exp tenv type))]

        [(assign-exp var exp) (begin0 (check 'Void) (type-of exp tenv (apply-tenv tenv var)))]

        [(symbol-exp sym)  (check 'Symbol)]
        [(real-exp   num)  (check 'Real)]
        [(bool-exp   #t)   (check 'True)]
        [(bool-exp   #f)   (check 'False)]
        [(char-exp   char) (check 'Char)]
        [(string-exp str)  (check 'String)]

        [(var-exp var)     (check (apply-tenv tenv var))]

        [(begin-exp exps)
         (let loop ([exp (car exps)] [next (cdr exps)])
           (cond [(null? next) (type-of exp tenv t0)]
                 [else
                  (type-of exp tenv #f)
                  (loop (car next) (cdr next))]))]

        [(if-exp pred-exp true-exp false-exp)
         (define tp (type-of pred-exp  tenv 'Boolean))
         (define tt (type-of true-exp  tenv t0))
         (define tf (type-of false-exp tenv t0))
         (case tp
           [(True)    tt]
           [(False)   tf]
           [(Boolean) (check (type-union tt tf))]
           [else
            (raise-arguments-error 'type-of   "type mismatch"
                                   "expected" 'Boolean
                                   "given"    tp
                                   "in"       pred-exp)])]


        #;[(trace-proc-exp vars body) (type-of (proc-exp vars body) tenv)]
        [(proc-exp vars body) (assert t0)]
        [(call-exp rator rands)
         (: ts (Listof Type))
         (define ts
           (if (list? rands)
               (for/list : (Listof Type)
                         ([rand (in-list rands)])
                 (type-of rand tenv #f))
               (let loop ([t (type-of rands tenv #f)]
                          [res : (Listof Type) '()])
                 (match t
                   ['Null (reverse res)]
                   [`(Pair ,A ,B)
                    (loop B (cons A res))]))))

         (match rator
           [(proc-exp vars body)
            (type-of body
                     (if (list? vars)
                         (extend-tenv* vars ts tenv)
                         (extend-tenv  vars (desugar-type `(List ,@ts)) tenv))
                     t0)]
           [_
            (match (type-of rator tenv #f)
              [`[-> (Values ,ts0 ... ,t* *) ,t1]
               #:when (and (types? ts0) (type? t*) (type? t1))
               (for ([t0 (in-list (append ts0 (build-list (- (length ts) (length ts0)) (const t*))))]
                     [t  (in-list ts)])
                 (<=: t t0))
               (check t1)]
              [`[-> (Values ,ts0 ...) ,t1]
               #:when (and (types? ts0) (type? t1))
               (for ([t0 (in-list ts0)]
                     [t  (in-list ts)])
                 (<=: t t0))
               (check t1)])])])))

  )
