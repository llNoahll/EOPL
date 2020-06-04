#lang typed/racket


(: down [-> (Listof Any) (Listof (List Any))])
(define down
  (λ (lst)
    (if (null? lst)
        '()
        (cons (list (car lst))
              (down (cdr lst))))))


(displayln (down '(1 2 3)))
(displayln (down '((a) (fine) (idea))))
(displayln (down '(a (more (complicated)) object)))