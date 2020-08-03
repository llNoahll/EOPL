#lang typed/racket

(require "../types/types.rkt"
         "../ExpValues/values-sig.rkt"
         "../ExpValues/values-unit.rkt"
         "../PrimitiveProc/primitive-proc-sig.rkt"
         "../PrimitiveProc/primitive-proc-unit.rkt"
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
  (export values^ env^ proc^ primitive-proc^ exp^)
  (link   values@ env@ proc@ primitive-proc@ exp@))

(define-values/invoke-unit base@
  (import)
  (export values^ env^ proc^ primitive-proc^ exp^))


(base-env (extend-env 'apply
                      (proc-val (procedure '(func args)
                                           (call-exp (var-exp 'func)
                                                     (var-exp 'args))
                                           (empty-env)))
                      (base-env)))

;; (base-env (extend-env 'Y
;;                       (*eval* '(λ (f)
;;                                  ((λ (recur-func)
;;                                     (recur-func recur-func))
;;                                   (λ (recur-func)
;;                                     (f (λ args
;;                                          (apply (recur-func recur-func) args))))))
;;                               (base-env))
;;                       (base-env)))
