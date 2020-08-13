#lang racket

(require "../types/types.rkt"
         "../ExpValues/values-sig.rkt"
         "../Environment/env-sig.rkt"
         "../Procedure/proc-sig.rkt"
         "primitive-proc-sig.rkt")

(provide primitive-proc@)


(define-unit primitive-proc@
  (import values^ env^ proc^)
  (export primitive-proc^)

  (define base-env (make-parameter (empty-env)))
  (define primitive-proc-table (make-hasheqv))

  )
