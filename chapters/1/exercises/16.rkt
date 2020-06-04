#lang typed/racket


(: invert (All (A B) [-> (Listof (List A B))
                         (Listof (List B A))]))
(define invert
  (λ (lst)
    (if (null? lst)
        '()
        (cons (list (cadar lst)
                    (caar lst))
              (invert (cdr lst))))))


(invert '((a 1) (a 2) (1 b) (2 b)))