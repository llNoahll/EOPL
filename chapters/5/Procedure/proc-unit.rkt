#lang typed/racket

(require "../types/types.rkt"
         "../Expressions/exp-sig.rkt"
         "../Environment/env-sig.rkt"
         "proc-sig.rkt")

(provide proc@)


(define-unit proc@
  (import env^ exp^)
  (export proc^)

  (: proc? [-> Any Boolean : Proc])
  (define-predicate proc? Proc)

  (: procedure [-> (U Symbol (Listof Symbol)) Exp Env Proc])
  (define procedure
    (λ (vars body env)
      (make-proc vars
                 body
                 (extend-env-bind+ (free-binds (if (symbol? vars) (list vars) vars)
                                               body env)
                                   (empty-env))
                 )))


  (: trace-proc? [-> Any Boolean : Trace-Proc])
  (define-predicate trace-proc? Trace-Proc)

  (: trace-procedure [-> (U Symbol (Listof Symbol)) Exp Env Trace-Proc])
  (define trace-procedure
    (λ (vars body env)
      (make-trace-proc vars
                       body
                       (extend-env-bind+ (free-binds (if (symbol? vars) (list vars) vars)
                                                     body env)
                                         (empty-env))
                       )))

  (: apply-procedure/k [-> Proc (Listof DenVal) Cont ExpVal])
  (define apply-procedure/k
    (λ (proc vals cont)
      (: vars (U Symbol (Listof Symbol)))
      (define vars (proc-vars proc))

      (: result ExpVal)
      (define result undefined)

      (dynamic-wind
        (λ ()
          (when (trace-proc? proc)
            (displayln (format "enter: ~a\n"
                               (if (symbol? vars)
                                   (cons vars vals)
                                   (map (ann (λ (var val) (cons var val))
                                             [-> Symbol DenVal (Pair Symbol DenVal)])
                                        vars vals))))))
        (λ ()
          (set! result
            (value-of/k (proc-body proc)
                        (if (symbol? vars)
                            (extend-env  vars
                                         vals
                                         (proc-saved-env proc))
                            (extend-env* vars
                                         vals
                                         (proc-saved-env proc)))
                        cont))

          result)
        (λ ()
          (when (trace-proc? proc)
            (displayln (format "result: ~a\n" result)))))))


  (: free-binds [-> (Listof Symbol) Exp Env (Listof (Pair Symbol Ref))])
  (define free-binds
    (λ (vars exp env)
      (cond [(assign-exp? exp)
             (free-binds vars
                         (begin-exp (list (var-exp (assign-exp-var exp))
                                          (assign-exp-exp exp)))
                         env)]

            [(symbol-exp? exp) '()]
            [(const-exp? exp) '()]
            [(bool-exp? exp) '()]
            [(char-exp? exp) '()]
            [(string-exp? exp) '()]

            [(var-exp? exp)
             (let ([var : Symbol (var-exp-var exp)])
               (if (memq var vars)
                   '()
                   (list (cons var (apply-env-ref env var)))))]

            [(begin-exp? exp)
             (let loop : (Listof (Pair Symbol Ref))
                  ([exps  : (Listof Exp) (begin-exp-exps exp)]
                   [binds : (Listof (Pair Symbol Ref)) '()])
                  (if (null? exps)
                      binds
                      (loop (cdr exps)
                            (append (free-binds (append (map (inst car Symbol Ref)
                                                             binds) vars)
                                                (car exps)
                                                env)
                                    binds))))]

            [(if-exp? exp)
             (free-binds vars
                         (begin-exp (list (if-exp-pred-exp  exp)
                                          (if-exp-true-exp  exp)
                                          (if-exp-false-exp exp)))
                         env)]
            [(cond-exp? exp)
             (free-binds vars
                         (begin-exp (apply append (cond-exp-branches exp)))
                         env)]
            [(let-exp? exp)
             (let loop : (Listof (Pair Symbol Ref))
                  ([exps : (Listof Exp) (let-exp-bind-exps exp)]
                   [binds : (Listof (Pair Symbol Ref)) '()])
                  (if (null? exps)
                      (append (free-binds (append (map (inst car Symbol Ref)
                                                       binds)
                                                  (let-exp-bind-vars exp)
                                                  vars)
                                          (let-exp-body exp)
                                          env)
                              binds)
                      (loop (cdr exps)
                            (append (free-binds (append (map (inst car Symbol Ref)
                                                             binds)
                                                        vars)
                                                (car exps)
                                                env)
                                    binds))))]

            [(primitive-proc-exp? exp)
             (define exps (primitive-proc-exp-exps exp))
             (if (null? exps)
                 '()
                 (free-binds vars
                             (begin-exp exps)
                             env))]
            [(proc-exp? exp)
             (let ([proc-vars (proc-exp-vars exp)])
               (free-binds (if (symbol? proc-vars)
                               (cons proc-vars vars)
                               (append proc-vars vars))
                           (proc-exp-body exp)
                           env))]
            [(call-exp? exp)
             (let ([rator (call-exp-rator exp)]
                   [rands (call-exp-rands exp)])
               (free-binds vars
                           (begin-exp (if (var-exp? rands)
                                          (list rator rands)
                                          (cons rator rands)))
                           env))]

            [else (raise-argument-error 'free-binds "exp?" exp)])))

  )
