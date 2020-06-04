#lang typed/racket


(: product [-> (Listof Symbol) (Listof Symbol) (Listof (Listof Symbol))])
(define product
  (λ (sos1 sos2)
    (cond [(or (null? sos1) (null? sos2)) '()]
          [else (append (map
                         (λ ([s : Symbol])
                           (list (car sos1) s))
                         sos2)
                        (product (cdr sos1) sos2))])))


(product '(a b c) '(x y))
