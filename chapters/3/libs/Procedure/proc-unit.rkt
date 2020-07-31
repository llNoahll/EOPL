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

  (: procedure [-> (Listof Symbol) Exp Env Proc])
  (define procedure
    (λ (vars body env)
      (make-proc vars body env)))

  (: apply-procedure [-> Proc (Listof ExpVal) ExpVal])
  (define apply-procedure
    (λ (proc vals)
      (value-of (proc-body proc)
                (extend-env* (proc-vars proc)
                             vals
                             (proc-saved-env proc)))))

  )
