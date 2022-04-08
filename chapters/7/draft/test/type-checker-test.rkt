#lang typed/racket

(require "../base/base.rkt")


(define-namespace-anchor ns-anchor)
(define eval-ns (namespace-anchor->namespace ns-anchor))

(: *type-check* [-> S-Exp TEnv (Option Type) Type])
(define *type-check*
  (λ (code tenv t0)
    (: exp Exp)
    (define exp
      (assert (call-with-values
               (λ () (eval (parser (auto-ann code)) eval-ns))
               (λ args (car args)))
              exp?))
    (type-of exp tenv t0)))


(displayln "Start type checker test.\n")

(displayln "\n----------------------------------------------")
(pretty-display
 (*type-check*
  '(begin
     (define dio #f)

     (: noah String)
     (define noah "")

     (set! noah "Noah Ma"))
  (base-tenv) #f))

(displayln "\n----------------------------------------------")
(pretty-display
 (*type-check*
  '(begin
     (define dio #f)

     (define noah '||)

     (set! noah "Noah Ma"))
  (base-tenv) #f))
