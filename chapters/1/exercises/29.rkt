#lang typed/racket


(: sort [-> (Listof Integer) (Listof Integer)])
(define sort
  (λ (loi)
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


    (let ([len (length loi)])
      (cond [(= len 0) '()]
            [(= len 1) loi]
            [(= len 2)
             (if (< (car loi) (cadr loi))
                 (list (car loi) (cadr loi))
                 (list (cadr loi) (car loi)))]
            [else (merge (sort (take loi (quotient len 2)))
                         (sort (drop loi (quotient len 2))))]))))


(displayln (sort '(8 2 5 2 3)))