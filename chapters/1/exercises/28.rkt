#lang typed/racket


(: merge [-> (Listof Integer) (Listof Integer)
             (Listof Integer)])
(define merge
  (λ (loi1 loi2)
    (cond [(null? loi1) loi2]
          [(null? loi2) loi1]
          [else
           (let ([x (car loi1)]
                 [y (car loi2)])
             (if (>= x y)
                 (cons y (merge loi1 (cdr loi2)))
                 (cons x (merge (cdr loi1) loi2))))])))


(displayln (merge '(1 4) '(1 2 8)))
(displayln (merge '(35 62 81 90 91) '(3 83 85 90)))