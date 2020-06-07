#lang racket


(define-signature integer-sequence^
  (
   number->sequence
   current-element

   move-to-left
   move-to-right

   insert-to-left
   insert-to-right

   at-left-end?
   at-right-end?
   ))

(provide integer-sequence^)
