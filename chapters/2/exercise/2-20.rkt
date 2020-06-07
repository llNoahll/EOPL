#lang racket


(require "../bintree/bintree-unit-3.rkt"
         "../bintree/bintree-sig.rkt")


(define-values/invoke-unit bintree@
  (import)
  (export bintree^))


(define t1
  (insert-to-right 14
                   (insert-to-left 12
                                   (number->bintree 13))))

(displayln (number->bintree 13))
(displayln t1)
(displayln (move-to-left-son t1))
(displayln (move-up (move-to-left-son t1)))
(displayln (eq? t1 (move-up (move-to-left-son t1))))
(displayln (current-element (move-to-left-son t1)))
(displayln (at-leaf? (move-to-right-son (move-to-left-son t1))))
(displayln (insert-to-left 15 t1))
(displayln (insert-to-right 15 t1))

(displayln (at-root? t1))
(displayln (at-root? (move-to-left-son t1)))
(displayln (at-root? (move-up (move-to-left-son t1))))