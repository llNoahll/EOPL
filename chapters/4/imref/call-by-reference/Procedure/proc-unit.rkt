#lang typed/racket

(require "../types/types.rkt"
         "../Reference/ref-sig.rkt"
         "../Expressions/exp-sig.rkt"
         "../Environment/env-sig.rkt"
         "proc-sig.rkt")

(provide proc@)


(define-unit proc@
  (import ref^ env^ exp^)
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

  (: apply-procedure [-> Proc (Listof (U DenVal Ref)) ExpVal])
  (define apply-procedure
    (λ (proc args)
      (: vars (U Symbol (Listof Symbol)))
      (define vars (proc-vars proc))

      (: vals (Listof DenVal))
      (define vals (map (ann (λ (arg) (if (ref? arg) (deref arg) arg))
                             [-> (U DenVal Ref) DenVal])
                        args))

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
            (value-of (proc-body proc)
                      (if (symbol? vars)
                          (extend-env vars
                                      vals
                                      (proc-saved-env proc))
                          (foldl (ann (λ (var arg saved-env)
                                        (if (ref? arg)
                                            (extend-env-bind var arg saved-env)
                                            (extend-env var arg saved-env)))
                                      [-> Symbol (U DenVal Ref) Env Env])
                                 (proc-saved-env proc)
                                 vars
                                 args)
                          ;; (extend-env-bind* vars
                          ;;                   args
                          ;;                   (proc-saved-env proc))
                          )))

          result)
        (λ ()
          (when (trace-proc? proc)
            (displayln (format "result: ~a\n" result)))))))


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
         (define curr-free-binds (free-binds vars (car exps) env))
         (define next-exps (cdr exps))
         (if (null? next-exps)
             curr-free-binds
             (append curr-free-binds
                     (free-binds (append (map (inst car Symbol Ref) curr-free-binds) vars)
                                 (begin-exp next-exps)
                                 env)))]

        [(if-exp pred-exp true-exp false-exp)
         (free-binds vars
                     (begin-exp (list pred-exp true-exp false-exp))
                     env)]
        [(cond-exp branches)
         (free-binds vars
                     (begin-exp (apply append branches))
                     env)]

        [(let-exp bind-vars bind-exps body)
         (cond [(null? bind-exps) (free-binds vars body env)]
               [else
                (define args-free-binds (free-binds vars (begin-exp bind-exps) env))
                (define new-env (extend-env-bind+ args-free-binds env))
                (define body-free-binds (free-binds (append vars bind-vars) body new-env))

                (append args-free-binds body-free-binds)])]
        [(letrec-exp bind-vars bind-exps body)
         (define new-env
           (extend-env+ (map (ann (λ (var) (cons var undefined))
                                  [-> Symbol (Pair Symbol Undefined)])
                             vars)
                        env))

         (free-binds (append vars bind-vars) (begin-exp (cons body bind-exps)) new-env)]

        [(primitive-proc-exp _ exps)
         (if (null? exps)
             '()
             (free-binds vars (begin-exp exps) env))]
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
