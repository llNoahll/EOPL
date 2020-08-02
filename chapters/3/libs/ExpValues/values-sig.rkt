#lang typed/racket

(require "../types/types.rkt")

(provide values^)


(define-signature values^
  (
   [symbol-val : [-> Symbol DenVal]]
   [num-val    : [-> Integer DenVal]]
   [bool-val   : [-> Boolean DenVal]]
   [pair-val   : [-> (Pair DenVal DenVal) DenVal]]
   [list-val   : [-> (Listof DenVal) DenVal]]
   [proc-val   : [-> Proc DenVal]]

   [expval->num  : [-> ExpVal Integer]]
   [expval->bool : [-> ExpVal Boolean]]
   [expval->pair : [-> ExpVal (Pair DenVal DenVal)]]
   [expval->list : [-> ExpVal (Listof DenVal)]]
   [expval->proc   : [-> ExpVal Proc]]

   [expval->s-expval : [-> ExpVal Any]]
   ))
