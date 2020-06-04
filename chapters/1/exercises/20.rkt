#lang typed/racket


(define-type S-List (Listof S-Expr))
(define-type S-Expr (U Symbol S-List))

(: count-occurrences [-> Symbol S-List Natural])
(define count-occurrences
  (λ (s s-list)
    (: count-occurrences-iter [-> Symbol S-List Natural Natural])
    (define count-occurrences-iter
      (λ (s s-list num)
        (cond [(null? s-list) num]
              [(symbol? (car s-list))
               (count-occurrences-iter s (cdr s-list)
                                       (if (eqv? (car s-list) s)
                                           (add1 num) num))]
              [else (+ (count-occurrences-iter
                        s (car s-list) num)
                       (count-occurrences-iter
                        s (cdr s-list) num))])))

    (count-occurrences-iter s s-list 0)))



(displayln (count-occurrences 'x '((f x) y (((x z) x)))))
(displayln (count-occurrences 'w '((f x) y (((x z) x)))))
(displayln (count-occurrences 'x '((f x) y (((x z) () x)))))
