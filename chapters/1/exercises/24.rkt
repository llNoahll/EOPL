#lang typed/racket


(: every? (All (A) [-> [-> Any Boolean : A] (Listof Any)
                       Boolean]))
(define every?
  (λ (pred lst)
    (cond [(null? lst) #t]
          [(not (pred (car lst))) #f]
          [else (every? pred (cdr lst))])))


(displayln (every? number? '(a b c 3 e)))
(displayln (every? number? '(1 2 3 5 4)))