#lang typed/racket


(define-type S-List (Listof S-Expr))
(define-type S-Expr (U Symbol Number S-List))

(: list-set [-> S-List Natural S-Expr S-List])
(define list-set
  (λ (lst n x)
    (cond [(<= (length lst) n) lst]
          [(zero? n) (cons x (cdr lst))]
          [else
           (cons (car lst)
                 (list-set (cdr lst)
                           (sub1 n)
                           x))])))


(displayln (list-set '(a b c d) 2 '(1 2)))
(displayln (list-set '(a b c d) 3 '(1 5 10)))