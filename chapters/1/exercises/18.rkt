#lang typed/racket


(define-type S-List (Listof S-Expr))
(define-type S-Expr (U Symbol S-List))

(: swapper [-> Symbol Symbol S-List S-List])
(define swapper
  (λ (s-1 s-2 s-list)
    (: swapper-in-s-expr [-> Symbol Symbol S-Expr S-Expr])
    (define swapper-in-s-expr
      (λ (s-1 s-2 s-expr)
        (if (symbol? s-expr)
            (cond [(eqv? s-2 s-expr) s-1]
                  [(eqv? s-1 s-expr) s-2]
                  [else s-expr])
            (swapper s-1 s-2 s-expr))))

    (if (null? s-list)
        '()
        (map (λ ([s-expr : S-Expr])
               (swapper-in-s-expr s-1 s-2 s-expr))
             s-list))))


(displayln (swapper 'a 'd '(a b c d)))
(displayln (swapper 'a 'd '(a d () c d)))
(displayln (swapper 'x 'y '((x) y (z (x)))))
(displayln (swapper 'a 'b '((b c) (b () d))))
(displayln (swapper 'a 'b '((b c) (b () d) b)))
(displayln (swapper 'a 'b '((b c) (b () d) a)))
(displayln (swapper 'a 'b '((b c) (b () d) c)))
