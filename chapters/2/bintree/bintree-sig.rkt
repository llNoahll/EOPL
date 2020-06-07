#lang racket


(define-signature bintree^
  (
   number->bintree
   current-element

   move-up
   move-to-left-son
   move-to-right-son

   insert-to-left
   insert-to-right

   at-root?
   at-leaf?
   ))

(provide bintree^)
