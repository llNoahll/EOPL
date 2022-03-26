#lang typed/racket

(require "../types/types.rkt")

(provide auto-apply)


(: auto-apply [-> S-Exp S-Exp])
(define auto-apply
  (λ (code)
    (match code
      [(or `(,(or 'quote 'quasiquote) ,(? symbol?))
           (? boolean?)
           (? real?)
           (? string?)
           (? char?)
           (? symbol?)
           `(,(or (? λ?) (? trace-λ?))
             ,(? (or/c symbol? (listof? symbol?)))
             ,(? s-exp?)))
       code]

      [`(set! ,var ,exp)
       #:when (and (symbol? var) (s-exp? exp))
       `(apply (λ () (set! ,var ,(auto-apply exp))) '())]

      [`(begin ,exps ..2)
       #:when ((listof? s-exp?) exps)
       `(begin ,@(map auto-apply exps))]

      [`(if ,(? s-exp? pred-exp)
            ,(? s-exp? true-exp)
            ,(? s-exp? false-exp))
       `(apply (λ ()
                 (if ,(auto-apply pred-exp)
                     ,(auto-apply true-exp)
                     ,(auto-apply false-exp)))
               '())]


      [`(letrec ([,bind-vars ,bind-exps] ...) ,body-exp)
       #:when (and ((listof? symbol?) bind-vars)
                   ((listof? s-exp?)  bind-exps)
                   (s-exp? body-exp))
       `(letrec ,(map (ann (λ (var exp)
                             `[,var (apply (λ () ,(auto-apply exp)) '())])
                           [-> Symbol S-Exp (List Symbol S-Exp)])
                      bind-vars bind-exps)
          ,(auto-apply body-exp))]


      [`(apply ,op ,exps)
       #:when (and (s-exp? op) (s-exp? exps))
       `(apply ,(auto-apply op) ,(auto-apply exps))]
      [`(,op ,exps ...)
       #:when (and (s-exp? op) ((listof? s-exp?) exps))
       `(apply (λ () (,(auto-apply op) ,@(map auto-apply exps))) '())]

      )))
