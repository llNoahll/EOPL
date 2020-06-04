#lang typed/racket


(define-type S-List (Listof S-Expr))
(define-type S-Expr (U Symbol S-List))

(: flatten [-> S-List (Listof Symbol)])
(define flatten
  (λ (s-list)
    (cond [(null? s-list) '()]
          [(symbol? (car s-list))
           (cons (car s-list) (flatten (cdr s-list)))]
          [else (append (flatten (car s-list))
                        (flatten (cdr s-list)))])))
;;   (λ (s-list)
;;     (cond [(null? s-list) '()]
;;           [(null? (car s-list))
;;            (flatten (cdr s-list))]
;;           [(symbol? (car s-list))
;;            (cons (car s-list) (flatten (cdr s-list)))]
;;           [else
;;            (let ([now (caar s-list)])
;;            (if (symbol? now)
;;                (cons now
;;                      (flatten (cons (cdar s-list) (cdr s-list))))
;;                (flatten (cons (flatten (car s-list))
;;                               (flatten (cdr s-list))))))])))


(displayln (flatten '(a b c)))
(displayln (flatten '((a) () (b ()) () (c))))
(displayln (flatten '((a b) c (((d)) e))))
(displayln (flatten '(a b (() (c)))))