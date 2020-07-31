#lang typed/racket

(require "../types/types.rkt"
         "../ExpValues/values-sig.rkt"
         "../ExpValues/values-unit.rkt"
         "../Procedure/proc-sig.rkt"
         "../Procedure/proc-unit.rkt"
         "../Environment/env-sig.rkt"
         "../Environment/env-unit.rkt"
         "../Expressions/exp-sig.rkt"
         "../Expressions/exp-unit.rkt")

(provide (all-from-out "../types/types.rkt")
         (all-defined-out))


(define-compound-unit/infer base@
  (import)
  (export env^ values^ proc^ exp^)
  (link   env@ values@ proc@ exp@))

(define-values/invoke-unit base@
  (import)
  (export env^ values^ proc^ exp^))
