#lang typed/racket


(require "../integer-sequence/integer-sequence-unit.rkt")

(require/typed "../integer-sequence/integer-sequence-sig.rkt"
  [#:signature integer-sequence^
   (
    [number->sequence : [-> Integer NodeInSequence]]
    [current-element  : [-> NodeInSequence Integer]]

    [move-to-left  : [-> NodeInSequence NodeInSequence]]
    [move-to-right : [-> NodeInSequence NodeInSequence]]

    [insert-to-left  : [-> Integer NodeInSequence NodeInSequence]]
    [insert-to-right : [-> Integer NodeInSequence NodeInSequence]]

    [at-left-end?  : [-> NodeInSequence Boolean]]
    [at-right-end? : [-> NodeInSequence Boolean]]
    )])


(define-values/invoke-unit integer-sequence@
  (import)
  (export integer-sequence^))


(displayln (number->sequence 7))
(displayln (current-element '(6 (5 4 3 2 1) (7 8 9))))
(displayln (move-to-left '(6 (5 4 3 2 1) (7 8 9))))
(displayln (move-to-right '(6 (5 4 3 2 1) (7 8 9))))
(displayln (insert-to-left 13 '(6 (5 4 3 2 1) (7 8 9))))
(displayln (insert-to-right 13 '(6 (5 4 3 2 1) (7 8 9))))
