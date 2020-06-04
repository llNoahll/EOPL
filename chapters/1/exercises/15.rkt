#lang typed/racket

(: duple (All (A) [-> Natural A (Listof A)]))
(define duple
  (λ (n x)
    (if (zero? n)
        '()
        (cons x (duple (sub1 n) x)))))


(displayln (duple 2 3))
(displayln (duple 4 '(ha ha)))
(displayln (duple 0 '(blah)))