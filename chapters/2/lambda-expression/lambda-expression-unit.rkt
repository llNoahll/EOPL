#lang typed/racket


(require/typed "lambda-expression-sig.rkt"
  [#:signature lambda-expression^
   (
    ;; constructors
    [var-exp    : [-> Variable Lc-Exp]]
    [lambda-exp : [-> Variable Lc-Exp Lc-Exp]]
    [app-exp    : [-> Lc-Exp Lc-Exp Lc-Exp]]

    ;; predicates
    [var-exp?    : [-> Lc-Exp Boolean]]
    [lambda-exp? : [-> Lc-Exp Boolean]]
    [app-exp?    : [-> Lc-Exp Boolean]]

    ;; extractors
    [var-exp->var          : [-> Lc-Exp Variable]]
    [lambda-exp->bound-var : [-> Lc-Exp Variable]]
    [lambda-exp->body      : [-> Lc-Exp Lc-Exp]]
    [app-exp->rator        : [-> Lc-Exp Lc-Exp]]
    [app-exp->rand         : [-> Lc-Exp Lc-Exp]]
    )])

(provide lambda-expression@ Variable variable variable* Lc-Exp)


(define-new-subtype Variable (make-variable Symbol))
(define-type Lambda (U 'lambda 'λ))
(define-type Lc-Exp (U Variable
                       (List Lambda
                             (List Variable)
                             Lc-Exp)
                       (List Lc-Exp Lc-Exp)))


(: variable [-> Symbol Variable])
(define variable
  (λ (sym)
    (if (or (eqv? sym 'lambda)
            (eqv? sym 'λ))
        (raise-argument-error 'variable "variable?" sym)
        (make-variable sym))))

(: variable* [-> (U Symbol (Listof Symbol))
                 (U Variable (Listof Variable))])
(define variable*
  (λ (arg)
    (if (symbol? arg)
        (variable arg)
        (map (λ ([sym : Symbol]) : Variable
                 (variable sym))
             arg))))


(: variable? [-> Any Boolean])
(define variable?
  (λ (arg)
    (and (symbol? arg)
         (not (lambda? arg)))))


(: lc-exp? [-> Any Boolean])
(define lc-exp?
  (λ (arg)
    (match arg
      [(? lambda?) #f]
      [(? symbol?) #t]
      [(list (? lambda?)
             (list (? variable?))
             (? lc-exp?))
       #t]
      [(list (? lc-exp?) (? lc-exp?)) #t]
      [_ #f])))


(: lambda? [-> Any Boolean : Lambda])
(define-predicate lambda? Lambda)


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
    (λ (exp-1 exp-2)
      `(,exp-1 ,exp-2)))


  ;; predicates
  (: var-exp? [-> Lc-Exp Boolean])
  (define var-exp?
    (λ (exp)
      (match exp
        [(? lambda?) #f]
        [(? symbol?) #t]
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


  ;; extractors
  (: var-exp->var [-> Lc-Exp Variable])
  (define var-exp->var
    (λ (exp)
      (match exp
        [(? symbol? var) (variable var)]
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
