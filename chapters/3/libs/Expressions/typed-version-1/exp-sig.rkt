#lang typed/racket

(require "../../types/version-1.rkt")

(provide exp^)


(define-signature exp^
  (
   [const-exp : [-> Integer Exp]]

   [nullary-exp : [-> Symbol Exp]]
   [unary-exp   : [-> Symbol Exp Exp]]
   [binary-exp  : [-> Symbol Exp Exp Exp]]
   [n-ary-exp   : [-> Symbol Exp * Exp]]

   [if-exp  : [-> Exp Exp Exp Exp]]
   [var-exp : [-> Symbol Exp]]
   [let-exp : [-> Symbol Exp Exp Exp]]

   [value-of : [-> Exp Env ExpVal]]
   ))
