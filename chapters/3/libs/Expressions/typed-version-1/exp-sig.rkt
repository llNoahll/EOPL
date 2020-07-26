#lang typed/racket

(require "../../types/version-1.rkt")

(provide exp^)


(define-signature exp^
  (
   [symbol-exp : [-> Symbol  Exp]]
   [const-exp  : [-> Integer Exp]]
   [bool-exp   : [-> Boolean Exp]]

   [nullary-exp : [-> Symbol Exp]]
   [unary-exp   : [-> Symbol Exp Exp]]
   [binary-exp  : [-> Symbol Exp Exp Exp]]
   [n-ary-exp   : [-> Symbol Exp * Exp]]

   [if-exp   : [-> Exp Exp Exp Exp]]
   [cond-exp : [-> (Listof (Pair Exp (Listof Exp))) Exp]]
   [var-exp  : [-> Symbol Exp]]
   [let-exp  : [-> (Listof Symbol) (Listof Exp) Exp Exp]]

   [value-of : [-> Exp Env ExpVal]]
   ))
