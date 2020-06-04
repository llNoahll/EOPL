#lang racket


(define-signature integer-number^
  (zero
   is-zero?
   successor
   predecessor))

(provide integer-number^)