#lang typed/racket


(: exists? (All (A) [-> [-> Any Boolean : A] (Listof Any)
                       Boolean]))
(define exists?
  (λ (pred lst)
    (cond [(null? lst) #f]
          [(pred (car lst)) #t]
          [else (exists? pred (cdr lst))])))


(displayln (exists? number? '(a b c 3 d)))
(displayln (exists? number? '(a b c d e)))