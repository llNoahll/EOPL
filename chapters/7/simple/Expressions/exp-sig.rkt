#lang typed/racket

(require "../types/types.rkt")

(provide exp^)


(define-signature exp^
  (
   [value-of/k : [-> Exp Env Cont FinalAnswer]]
   [type-of    : [-> Exp TEnv (Option Type) Type]]
   ))
