#lang typed/racket


(: sort/predicate [-> [-> Integer Integer Boolean] (Listof Integer)
                      (Listof Integer)])
(define sort/predicate
  (λ (pred loi)
    (: merge [-> [-> Integer Integer Boolean]
                 (Listof Integer) (Listof Integer)
                 (Listof Integer)])
    (define merge
      (λ (pred loi1 loi2)
        (cond [(null? loi1) loi2]
              [(null? loi2) loi1]
              [else
               (let ([x (car loi1)]
                     [y (car loi2)])
                 (if (pred x y)
                     (cons x (merge pred (cdr loi1) loi2))
                     (cons y (merge pred loi1 (cdr loi2)))))])))


    (let ([len (length loi)])
      (cond [(= len 0) '()]
            [(= len 1) loi]
            [(= len 2)
             (if (pred (car loi) (cadr loi))
                 (list (car loi) (cadr loi))
                 (list (cadr loi) (car loi)))]
            [else (merge pred
                         (sort/predicate pred (take loi (quotient len 2)))
                         (sort/predicate pred (drop loi (quotient len 2))))]))))


(displayln (sort/predicate < '(8 2 5 2 3)))
(displayln (sort/predicate > '(8 2 5 2 3)))
