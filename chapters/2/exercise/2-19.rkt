#lang typed/racket


(require "../bintree/bintree-unit-1.rkt")

(require/typed "../bintree/bintree-sig.rkt"
  [#:signature bintree^
   (
    [number->bintree : [-> Integer BinTree]]
    [current-element : [-> BinTree Integer]]

    [move-to-left-son  : [-> BinTree BinTree]]
    [move-to-right-son : [-> BinTree BinTree]]

    [insert-to-left  : [-> Integer BinTree BinTree]]
    [insert-to-right : [-> Integer BinTree BinTree]]

    [at-leaf? : [-> BinTree Boolean]]
    )])


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
(displayln (current-element (move-to-left-son t1)))
(displayln (at-leaf? (move-to-right-son (move-to-left-son t1))))
(displayln (insert-to-left 15 t1))
(displayln (insert-to-right 15 t1))
