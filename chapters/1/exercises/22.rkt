#lang typed/racket


(: filter-in (All (A) [-> [-> Any Boolean : A] (Listof Any)
                          (Listof A)]))
(define filter-in
  (λ (pred lst)
    (cond [(null lst) '()]
          [(pred (car lst))
           (cons (car lst)
                 (filter-in pred (cdr lst)))]
          [else (filter-in pred (cdr lst))])))


(displayln (filter-in number? '(a 2 (1 3) b 7)))
(displayln (filter-in symbol? '(a (b c) 17 foo)))