#lang typed/racket


(require "lambda-expression-sig.rkt")

(provide lambda-expression@)


(define-unit lambda-expression@
  (import)
  (export lambda-expression^)

  ;; constructors
  (: var-exp [-> Variable Lc-Exp])
  (define var-exp (λ (var) var))

  (: lambda-exp [-> Variable Lc-Exp Lc-Exp])
  (define lambda-exp
    (λ (var body)
      `(λ (,var) ,body)))

  (: app-exp [-> Lc-Exp Lc-Exp Lc-Exp])
  (define app-exp
    (λ (rator rand)
      `(,rator ,rand)))


  ;; predicates
  (: var-exp? [-> Lc-Exp Boolean])
  (define var-exp?
    (λ (exp)
      (match exp
        [(? variable?) #t]
        [_ #f])))

  (: lambda-exp? [-> Lc-Exp Boolean])
  (define lambda-exp?
    (λ (exp)
      (match exp
        [(list (? lambda?)
               (list (? variable?))
               (? lc-exp?))
         #t]
        [_ #f])))

  (: app-exp? [-> Lc-Exp Boolean])
  (define app-exp?
    (λ (exp)
      (match exp
        [(list (? lc-exp?) (? lc-exp?)) #t]
        [_ #f])))

  (: lc-exp? [-> Any Boolean])
  (define lc-exp?
    (λ (arg)
      (match arg
        [(? variable?) #f]
        [(list (? lambda?)
               (list (? variable?))
               (? lc-exp?))
         #t]
        [(list (? lc-exp?) (? lc-exp?)) #t]
        [_ #f])))


  ;; extractors
  (: var-exp->var [-> Lc-Exp Variable])
  (define var-exp->var
    (λ (exp)
      (match exp
        [(? variable? var) (variable var)]
        [_ (raise-argument-error 'var-exp->var "var-exp?" exp)])))

  (: lambda-exp->bound-var [-> Lc-Exp Variable])
  (define lambda-exp->bound-var
    (λ (exp)
      (match exp
        [(list (? lambda?)
               (list (? variable? bound-var))
               (? lc-exp?))
         bound-var]
        [_ (raise-argument-error 'lambda-exp->bound-var "lambda-exp?" exp)])))

  (: lambda-exp->body [-> Lc-Exp Lc-Exp])
  (define lambda-exp->body
    (λ (exp)
      (match exp
        [(list (? lambda?)
               (list (? variable?))
               (? lc-exp? body))
         body]
        [_ (raise-argument-error 'lambda-exp->body "lambda-exp?" exp)])))

  (: app-exp->rator [-> Lc-Exp Lc-Exp])
  (define app-exp->rator
    (λ (exp)
      (match exp
        [(list (? lc-exp? rator) (? lc-exp?))
         rator]
        [_ (raise-argument-error 'app-exp->rator "app-exp?" exp)])))

  (: app-exp->rand [-> Lc-Exp Lc-Exp])
  (define app-exp->rand
    (λ (exp)
      (match exp
        [(list (? lc-exp?) (? lc-exp? rand))
         rand]
        [_ (raise-argument-error 'app-exp->rand "app-exp?" exp)]))))
