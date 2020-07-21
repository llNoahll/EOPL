#lang typed/racket

(provide (all-defined-out))


(define-type S-List (Listof S-Expr))
(define-type S-Expr (U Boolean Integer Symbol S-List))


(: parser [-> S-Expr S-Expr])
(define parser
  (λ (code)
    (match code
      [(? integer? num) `(const-exp ,num)]
      [(? symbol? var) `(var-exp ',var)]

      [`(if ,pred-exp ,true-exp ,false-exp)
       `(if-exp ,(parser pred-exp)
                ,(parser true-exp)
                ,(parser false-exp))]
      [`(let ([,(? symbol? bound-var)
               ,bound-exp])
          ,body-exp)
       `(let-exp ',bound-var ,(parser bound-exp)
                 ,(parser body-exp))]

      [`(,op) `(nullary-exp ',op)]
      [`(,op ,exp) `(unary-exp ',op ,(parser exp))]
      [`(,op ,exp-1 ,exp-2) `(binary-exp ',op
                                         ,(parser exp-1)
                                         ,(parser exp-2))]
      [`(,op ,exps ...) `(n-ary-exp ',op
                                    ,@(map parser (cast exps (Listof S-Expr))))])))
