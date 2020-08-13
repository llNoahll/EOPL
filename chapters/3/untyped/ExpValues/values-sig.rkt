#lang racket

(require "../types/types.rkt")

(provide values^)


(define-signature values^
  (
   symbol-val
   num-val
   bool-val
   char-val
   string-val
   pair-val
   list-val
   proc-val

   expval->symbol
   expval->num
   expval->bool
   expval->char
   expval->string
   expval->pair
   expval->list
   expval->proc

   expval->denval
   expval->s-expval
   ))
