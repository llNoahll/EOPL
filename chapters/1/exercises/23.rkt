#lang typed/racket


(: list-index (All (A) [-> [-> Any Boolean : A] (Listof Any)
                           (U Natural Boolean)]))
(define list-index
  (λ (pred lst)
    (: list-index-iter (All (A) [-> [-> Any Boolean : A] (Listof Any) Natural
                                    (U Natural Boolean)]))
    (define list-index-iter
      (λ (pred lst num)
        (cond [(null? lst) #f]
              [(pred (car lst)) num]
              [else (list-index-iter
                     pred (cdr lst) (add1 num))])))

    (list-index-iter pred lst 0)))


(displayln (list-index number? '(a 2 (1 3) b 7)))
(displayln (list-index symbol? '(a (b c) 17 foo)))
(displayln (list-index symbol? '(1 2 (a b) 3)))