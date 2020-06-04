#lang typed/racket


(require "../lambda-expression/lambda-expression-unit.rkt")

(require/typed "../lambda-expression/lambda-expression-sig.rkt"
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


(define-values/invoke-unit lambda-expression@
  (import)
  (export lambda-expression^))


(: occurs-free? [-> Variable Lc-Exp Boolean])
(define occurs-free?
  (λ (search-var exp)
    (cond [(var-exp? exp)
           (eqv? search-var (var-exp->var exp))]
          [(lambda-exp? exp)
           (and (not (eqv? search-var
                           (lambda-exp->bound-var exp)))
                (occurs-free? search-var (lambda-exp->body exp)))]
          [else (or (occurs-free? search-var (app-exp->rator exp))
                    (occurs-free? search-var (app-exp->rand  exp)))])))

(: occurs-free?-test [-> Variable Lc-Exp Void])
(define occurs-free?-test
  (λ (search-var exp)
    (displayln "--------------------------")
    (displayln search-var)
    (displayln exp)
    (displayln (occurs-free? search-var exp))
    (displayln "--------------------------")
    (newline)))


(occurs-free?-test (variable 'x) (variable 'x))
(occurs-free?-test (variable 'x) (variable 'y))
(occurs-free?-test (variable 'x)
                   (lambda-exp (variable 'x)
                               (app-exp (variable 'x)
                                        (variable 'y))))
(occurs-free?-test (variable 'x)
                   (lambda-exp (variable 'y)
                               (app-exp (variable 'x)
                                        (variable 'y))))
(occurs-free?-test (variable 'x)
                   (app-exp (lambda-exp (variable 'x)
                                        (variable 'x))
                            (app-exp (variable 'x)
                                     (variable 'y))))
(occurs-free?-test (variable 'x)
                   (lambda-exp (variable 'y)
                               (lambda-exp (variable 'z)
                                           (app-exp (variable 'x)
                                                    (app-exp (variable 'y)
                                                             (variable 'z))))))
