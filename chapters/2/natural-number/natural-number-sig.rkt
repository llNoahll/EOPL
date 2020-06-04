#lang racket


(define-signature natural-number^
  (zero
   is-zero?
   ;; is-pos-int?
   ;; is-natural?
   successor
   predecessor))

(provide natural-number^)