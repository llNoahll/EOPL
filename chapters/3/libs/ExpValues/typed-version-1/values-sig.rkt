#lang typed/racket

(require "../../types/version-1.rkt")

(provide values^)


(define-signature values^
  (
   [num-val  : [-> Integer ExpVal]]
   [bool-val : [-> Boolean ExpVal]]
   [pair-val : [-> (Pair DenVal DenVal) ExpVal]]
   [list-val : [-> (Listof DenVal) ExpVal]]

   [expval->num  : [-> ExpVal Integer]]
   [expval->bool : [-> ExpVal Boolean]]
   [expval->pair : [-> ExpVal (Pair DenVal DenVal)]]
   [expval->list : [-> ExpVal (Listof DenVal)]]
   ))
