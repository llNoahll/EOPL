#lang typed/racket


(: number-elements [-> (Listof Any) (Listof (List Natural Any))])
(define number-elements
  (λ (lst)
    (: g [-> (List Natural Any) (Listof (List Natural Any))
             (Listof (List Natural Any))])
    (define g
      (λ (head tail)
        (if (null? tail)
            (list head)
            (cons head
                  (map
                   (λ ([n-v : (List Natural Any)])
                     (list (add1 (car n-v))
                           (cadr n-v)))
                   tail)))))

    (if (null? lst)
        '()
        (g (list 0 (car lst))
           (number-elements (cdr lst))))))


(number-elements '(v0 v1 v2 v3 v4 v5))