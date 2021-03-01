#lang typed/racket

(require "../types/types.rkt"
         "../ExpValues/values-sig.rkt"
         "../Environment/env-sig.rkt"
         "../Procedure/proc-sig.rkt"
         "primitive-proc-sig.rkt")

(provide primitive-proc@)


(define-unit primitive-proc@
  (import values^ env^ proc^)
  (export primitive-proc^)

  (: base-env (Parameter Env))
  (define base-env (make-parameter (empty-env)))


  (: primitive-proc-table (Mutable-HashTable Symbol [-> DenVal * ExpVal]))
  (define primitive-proc-table (make-hasheqv))

  )
