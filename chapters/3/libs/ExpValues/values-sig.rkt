#lang racket

(provide values^)


(define-signature values^
  (
   symbol-val
   num-val
   bool-val
   pair-val
   list-val

   expvel-num
   expval-bool
   expval-pair
   expval-list
   ))
