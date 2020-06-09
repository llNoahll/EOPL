#lang typed/racket

(require "../../types/version-1.rkt")

(provide values^)


(define-signature values^
  (
   [num-val  : [-> Integer ExpVal]]
   [bool-val : [-> Boolean ExpVal]]

   [expval->num  : [-> ExpVal Integer]]
   [expval->bool : [-> ExpVal Boolean]]
   ))
