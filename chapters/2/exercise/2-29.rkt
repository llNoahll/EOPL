#lang typed/racket


(require "../lambda-expression/typed-lambda-expression-version-3/lambda-expression-sig.rkt"
         "../lambda-expression/typed-lambda-expression-version-3/lambda-expression-unit.rkt"
         "../parser/typed-parse-unparse-version-2/parse-unparse-sig.rkt"
         "../parser/typed-parse-unparse-version-2/parse-unparse-unit.rkt")

(define-values/invoke-unit lambda-expression@
  (import)
  (export lambda-expression^))

(define-values/invoke-unit parse-unparse@
  (import lambda-expression^)
  (export parse-unparse^))


(: occurs-free? [-> Variable Lc-Exp Boolean])
(define occurs-free?
  (λ (search-var exp)
    (cond [(var-exp? exp)
           (eqv? search-var (var-exp->var exp))]
          [(lambda-exp? exp)
           (and (not (ormap (λ ([var-exp : Var-Exp])
                              (eqv? search-var (var-exp->var var-exp)))
                            (lambda-exp->bound-vars exp)))
                (occurs-free? search-var (lambda-exp->body exp)))]
          [else (or (occurs-free? search-var (app-exp->rator exp))
                    (ormap (λ ([exp : Lc-Exp])
                             (occurs-free? search-var exp))
                           (app-exp->rands exp)))])))

(: occurs-free?-test [-> Variable Lc-Exp Void])
(define occurs-free?-test
  (λ (search-var exp)
    (displayln "--------------------------")
    (displayln search-var)
    (displayln exp)
    (displayln (occurs-free? search-var exp))
    (displayln "--------------------------")
    (newline)))

(var-exp (variable 'x))
(occurs-free?-test (variable 'x) (var-exp (variable 'x)))
(occurs-free?-test (variable 'x) (var-exp (variable 'y)))
(occurs-free?-test (variable 'x)
                   (lambda-exp (list (var-exp (variable 'x)))
                               (app-exp (var-exp (variable 'x))
                                        (var-exp (variable 'y)))))
(occurs-free?-test (variable 'x)
                   (lambda-exp (list (var-exp (variable 'y)))
                               (app-exp (var-exp (variable 'x))
                                        (var-exp (variable 'y)))))
(occurs-free?-test (variable 'x)
                   (app-exp (lambda-exp (list (var-exp (variable 'x)))
                                        (var-exp (variable 'x)))
                            (app-exp (var-exp (variable 'x))
                                     (var-exp (variable 'y)))))
(occurs-free?-test (variable 'x)
                   (lambda-exp (list (var-exp (variable 'y)))
                               (lambda-exp (list (var-exp (variable 'z)))
                                           (app-exp (var-exp (variable 'x))
                                                    (app-exp (var-exp (variable 'y))
                                                             (var-exp (variable 'z)))))))

(unparse-lc-exp (lambda-exp (list (var-exp (variable 'y)) (var-exp (variable 'x)))
                            (lambda-exp (list (var-exp (variable 'z)))
                                        (app-exp (var-exp (variable 'x))
                                                 (app-exp (var-exp (variable 'y))
                                                          (var-exp (variable 'z)))))))


(displayln (parse-expression '(λ (x z) ((x x) (λ (z x) (z x))))))
(displayln (unparse-lc-exp (parse-expression '(λ (x z) ((x x) (λ (z x) (z x)))))))

(displayln (parse-expression '(a b c)))
(displayln (unparse-lc-exp (parse-expression '(a b c))))


(displayln (parse-expression '(λ)))
(displayln (parse-expression '(λ (x))))
