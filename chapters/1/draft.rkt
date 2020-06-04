#! /usr/bin/env racket
#lang racket/base

;; (require racket/fixnum)

;; (define (list-head orig-lst orig-pos)
;;   (unless (and (fixnum? orig-pos) (>= orig-pos 0))
;;     (error 'list-head "invalid index ~s" orig-pos))
;;   (let loop [(lst orig-lst) [pos orig-pos]]
;;     (cond [(fx<= pos 1)
;;            (if (fx= pos 0)
;;                '()
;;                (if (pair? lst)
;;                    (list (car lst))
;;                    (error 'list-head
;;                           "~s is not a proper list" orig-lst)))]
;;           [(pair? lst)
;;            (let ([a (car lst)] [ls (cdr lst)])
;;              (if (pair? ls)
;;                  (list* a (car ls) (loop (cdr ls) (fx- pos 2)))
;;                  (error 'list-head
;;                         "~s is not a proper list" orig-lst)))]
;;           [else (error 'list-head
;;                        "~s is not a proper list" orig-lst)])))


(displayln (list-tail '(9 1 33 0 1) 3))
(displayln (list-head '(9 1 33 0 1) 3))

