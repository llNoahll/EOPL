#lang typed/racket


(require "lambda-expression-sig.rkt")

(provide lambda-expression@
         Variable variable variable?
         Lambda lambda? (rename-out [lambda? λ?])
         Var-Exp Lambda-Exp App-Exp Lc-Exp)


(define-unit lambda-expression@
  (import)
  (export lambda-expression^)

  ;; constructors
  (: var-exp [-> Variable Var-Exp])
  (define var-exp
    (λ (var)
      (struct-var-exp var)))

  (: lambda-exp [-> (Listof Var-Exp) Lc-Exp Lambda-Exp])
  (define lambda-exp
    (λ (bound-vars body)
      (struct-lambda-exp bound-vars body)))

  (: app-exp [-> Lc-Exp Lc-Exp * App-Exp])
  (define app-exp
    (λ (rator . rands)
      (struct-app-exp rator rands)))


  ;; predicates
  (: var-exp? [-> Any Boolean : Var-Exp])
  (define-predicate var-exp? Var-Exp)

  (: lambda-exp? [-> Any Boolean : Lambda-Exp])
  (define-predicate lambda-exp? Lambda-Exp)

  (: app-exp? [-> Any Boolean : App-Exp])
  (define-predicate app-exp? App-Exp)

  (: lc-exp? [-> Any Boolean : Lc-Exp])
  (define-predicate lc-exp? Lc-Exp)


  ;; extractors
  (: var-exp->var [-> Var-Exp Variable])
  (define var-exp->var
    (λ (exp)
      (struct-var-exp-var exp)))

  (: lambda-exp->bound-vars [-> Lambda-Exp (Listof Var-Exp)])
  (define lambda-exp->bound-vars
    (λ (exp)
      (struct-lambda-exp-vars exp)))

  (: lambda-exp->body [-> Lambda-Exp Lc-Exp])
  (define lambda-exp->body
    (λ (exp)
      (struct-lambda-exp-body exp)))

  (: app-exp->rator [-> App-Exp Lc-Exp])
  (define app-exp->rator
    (λ (exp)
      (struct-app-exp-rator exp)))

  (: app-exp->rands [-> App-Exp (Listof Lc-Exp)])
  (define app-exp->rands
    (λ (exp)
      (struct-app-exp-rands exp))))
