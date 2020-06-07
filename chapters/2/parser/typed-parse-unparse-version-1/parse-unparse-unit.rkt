#lang typed/racket


(require "parse-unparse-sig.rkt"
         "../../lambda-expression/typed-lambda-expression-version-2/lambda-expression-sig.rkt")


(provide parse-unparse@)


(define-unit parse-unparse@
  (import lambda-expression^)
  (export parse-unparse^)

  (: parse-expression [-> Any Lc-Exp])
  (define parse-expression
    (λ (datum)
      (match datum
        [(? symbol?) (var-exp (variable datum))]
        [(list (? λ?)
               (list (? symbol? var))
               body)
         (lambda-exp (var-exp (variable var))
                     (parse-expression body))]
        [`(,rator ,rand)
         (app-exp (parse-expression rator)
                  (parse-expression rand))]
        [_ (raise-argument-error 'parser-expression "concrete-syntax?" datum)])))

  (: unparse-lc-exp [-> Lc-Exp Any])
  (define unparse-lc-exp
    (λ (exp)
      (match exp
        [(? var-exp?) (var-exp->var exp)]
        [(? lambda-exp?)
         (list 'λ
               (list (unparse-lc-exp (lambda-exp->bound-var exp)))
               (unparse-lc-exp (lambda-exp->body exp)))]
        [(? app-exp?)
         (list (unparse-lc-exp (app-exp->rator exp))
               (unparse-lc-exp (app-exp->rand  exp)))])))
  )
