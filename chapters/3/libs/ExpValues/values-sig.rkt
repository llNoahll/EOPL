#lang racket

(provide values^)


(define-signature values^
  (
   symbol-val
   num-val
   bool-val
   pair-val
   list-val

   expval->num
   expval->bool
   expval->pair
   expval->list

   expval->s-exp
   ))
