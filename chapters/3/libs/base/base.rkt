#lang typed/racket

(require "../types/types.rkt"
         "../Parse/parser.rkt"
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
  (export values^ env^ proc^ exp^ primitive-proc^)
  (link   values@ env@ proc@ exp@ primitive-proc@))

(define-values/invoke-unit base@
  (import)
  (export values^ env^ proc^ exp^ primitive-proc^))


(define-namespace-anchor ns-anchor)
(define eval-ns (namespace-anchor->namespace ns-anchor))

(: *eval* [-> S-Exp Env ExpVal])
(define *eval*
  (λ (code env)
    (: exp Exp)
    (define exp
      (cast (call-with-values
             (λ () (eval (parser code) eval-ns))
             (λ args (car args)))
            Exp))

    ;; (pretty-print code)
    (value-of exp env)))


(base-env (extend-env 'apply
                      (proc-val (procedure '(func args)
                                           (call-exp (var-exp 'func)
                                                     (var-exp 'args))
                                           (empty-env)))
                      (base-env)))

(base-env (extend-env 'Y
                      (cast (*eval* '(λ (f)
                                       ((λ (recur-func)
                                          (recur-func recur-func))
                                        (λ (recur-func)
                                          (f (λ args
                                               (apply (recur-func recur-func) args))))))
                                    (base-env))
                            DenVal)
                      (base-env)))

(base-env (extend-env 'map
                      (cast (*eval* '(Y (λ (map)
                                          (λ (func ls)
                                            (if (null? ls)
                                                '()
                                                (cons (func (car ls))
                                                      (map func (cdr ls)))))))
                                    (base-env))
                            DenVal)
                      (base-env)))

(base-env (extend-env 'Y*
                      (cast (*eval* '(λ funcs
                                       ((λ (recur-funcs)
                                          (recur-funcs recur-funcs))
                                        (λ (recur-funcs)
                                          (map (λ (func)
                                                 (λ args
                                                   (apply (apply func (recur-funcs recur-funcs)) args)))
                                               funcs))))
                                    (base-env))
                            DenVal)
                      (base-env)))
