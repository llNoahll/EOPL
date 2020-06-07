#lang typed/racket


(require "parse-unparse-sig.rkt"
         "../../lambda-expression/typed-lambda-expression-version-3/lambda-expression-sig.rkt")

(provide parse-unparse@)


(define-unit parse-unparse@
  (import lambda-expression^)
  (export parse-unparse^)

  (: parse-expression [-> Any Lc-Exp])
  (define parse-expression
    (λ (datum)
      (match datum
        [(? variable?) (var-exp (variable (cast datum Symbol)))]
        [(list (? λ?)
               (list vars ...)
               body)
         (lambda-exp (map (λ ([var : Any]) : Var-Exp
                              (if (symbol? var)
                                  (var-exp (variable var))
                                  (raise-argument-error 'variable
                                                        "variable?" var)))
                          vars)
                     (parse-expression body))]
        [(list (? (compose1 not λ?) rator) rands ...)
         (apply app-exp
                (parse-expression rator)
                (map parse-expression rands))]
        [(? λ?)
         (raise-arguments-error 'parser-expression
                                "Lambda is invalid Symbol!"
                                "lambda" datum)]
        [(list (? λ?) _ ...)
         (raise-arguments-error 'parser-expression
                                "Lambda requires args and body!"
                                "lambda" datum)]
        [_ (raise-argument-error 'parser-expression "concrete-syntax?" datum)])))

  (: unparse-lc-exp [-> Lc-Exp Any])
  (define unparse-lc-exp
    (λ (exp)
      (match exp
        [(? var-exp?) (var-exp->var exp)]
        [(? lambda-exp?)
         (list 'λ
               (map unparse-lc-exp
                    (lambda-exp->bound-vars exp))
               (unparse-lc-exp (lambda-exp->body exp)))]
        [(? app-exp?)
         `(,(unparse-lc-exp (app-exp->rator exp))
           ,@(map unparse-lc-exp (app-exp->rands exp)))])))
  )
