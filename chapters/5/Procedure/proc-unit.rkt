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

  (: apply-procedure/k [-> Proc (Listof DenVal) Cont FinalAnswer])
  (define apply-procedure/k
    (λ (proc vals cont)
      (: vars (U Symbol (Listof Symbol)))
      (define vars (proc-vars proc))

      (when (trace-proc? proc)
        (displayln (format "enter: ~a\n"
                           (if (symbol? vars)
                               (cons vars vals)
                               (map (ann (λ (var val) (cons var val))
                                         [-> Symbol DenVal (Pair Symbol DenVal)])
                                    vars vals)))))

      (value-of/k (proc-body proc)
                  (if (symbol? vars)
                      (extend-env  vars
                                   vals
                                   (proc-saved-env proc))
                      (extend-env* vars
                                   vals
                                   (proc-saved-env proc)))
                  (ann (λ (result)
                         (when (trace-proc? proc)
                           (displayln (format "result: ~a\n" result)))
                         (cont result))
                       Cont))))


  (: free-binds [-> (Listof Symbol) Exp Env (Listof (Pair Symbol Ref))])
  (define free-binds
    (λ (vars exp env)
      (match exp
        [(assign-exp var exp)
         (free-binds vars
                     (begin-exp (list (var-exp var) exp))
                     env)]

        [(or (symbol-exp _) (const-exp _) (bool-exp _) (char-exp _) (string-exp _)) '()]

        [(var-exp var)
         (if (memq var vars)
             '()
             (list (cons var (apply-env-ref env var))))]

        [(begin-exp exps)
         (let loop : (Listof (Pair Symbol Ref))
              ([exps exps]
               [binds : (Listof (Pair Symbol Ref)) '()])
              (if (null? exps)
                  binds
                  (loop (cdr exps)
                        (append (free-binds (append (map (inst car Symbol Ref)
                                                         binds) vars)
                                            (car exps)
                                            env)
                                binds))))]

        [(if-exp pred-exp true-exp false-exp)
         (free-binds vars
                     (begin-exp (list pred-exp true-exp false-exp))
                     env)]
        [(cond-exp branches)
         (free-binds vars
                     (begin-exp (apply append branches))
                     env)]

        [(or (let-exp bind-vars exps body)
             (letrec-exp bind-vars exps body))
         #:when (not (false? exps))
         (let loop : (Listof (Pair Symbol Ref))
              ([exps exps]
               [binds : (Listof (Pair Symbol Ref)) '()])
              (if (null? exps)
                  (append (free-binds (append (map (inst car Symbol Ref)
                                                   binds)
                                              bind-vars
                                              vars)
                                      body
                                      env)
                          binds)
                  (loop (cdr exps)
                        (append (free-binds (append (map (inst car Symbol Ref)
                                                         binds)
                                                    vars)
                                            (car exps)
                                            env)
                                binds))))]

        [(primitive-proc-exp _ exps)
         (if (null? exps)
             '()
             (free-binds vars
                         (begin-exp exps)
                         env))]
        [(or (trace-proc-exp proc-vars body) (proc-exp proc-vars body))
         #:when (not (false? body))
         (free-binds (if (symbol? proc-vars)
                         (cons proc-vars vars)
                         (append proc-vars vars))
                     body
                     env)]
        [(call-exp rator rands)
         (free-binds vars
                     (begin-exp (if (var-exp? rands)
                                    (list rator rands)
                                    (cons rator rands)))
                     env)])))

  )
