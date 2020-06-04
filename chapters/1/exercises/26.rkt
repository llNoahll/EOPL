#lang typed/racket

(require "17.rkt")


(: up [-> (Listof Any) (Listof Any)])
(define up
  (λ (lst)
    (cond [(null? lst) '()]
          [(not (list? (car lst)))
           (cons (car lst) (up (cdr lst)))]
          [else (append (car lst)
                        (up (cdr lst)))])))
;; (define up
;;   (λ (lst)
;;     (if (null? lst)
;;         '()
;;         (let [(now (car lst))]
;;           (cond
;;             [(not (list? now))
;;              (cons now (up (cdr lst)))]
;;             [(null? now) (up (cdr lst))]
;;             [else
;;              (cons (car now)
;;                    (if (not (null? (cdr now)))
;;                        (up (cons (cdr now) (cdr lst)))
;;                        (up (cdr lst))))])))))


(displayln (up '((1 2) (3 4))))
(displayln (up '((x (y)) z)))
