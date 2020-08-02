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
      (make-proc vars body env)))

  (: apply-procedure [-> Proc (Listof DenVal) ExpVal])
  (define apply-procedure
    (λ (proc vals)
      (let ([vars : (U Symbol (Listof Symbol)) (proc-vars proc)])
        (value-of (proc-body proc)
                  (if (symbol? vars)
                      (extend-env vars
                                  vals
                                  (proc-saved-env proc))
                      (extend-env* vars
                                   vals
                                   (proc-saved-env proc)))))))

  )
