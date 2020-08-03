#lang typed/racket

(require "../types/types.rkt")

(provide exp^)


(define-signature exp^
  (
   [symbol-exp : [-> Symbol  Symbol-Exp]]
   [const-exp  : [-> Real Const-Exp]]
   [bool-exp   : [-> Boolean Bool-Exp]]
   [char-exp   : [-> Char Char-Exp]]
   [string-exp : [-> String String-Exp]]

   [if-exp   : [-> Exp Exp Exp If-Exp]]
   [cond-exp : [-> (Listof (Pair Exp (Listof Exp))) Cond-Exp]]
   [var-exp  : [-> Symbol Var-Exp]]
   [let-exp  : [-> (Listof Symbol) (Listof Exp) Exp Let-Exp]]

   [primitive-proc-exp : [-> Symbol Exp * Primitive-Proc-Exp]]

   [proc-exp : [-> (U Symbol (Listof Symbol)) Exp Proc-Exp]]
   [call-exp : [-> Exp (U Var-Exp (Listof Exp)) Call-Exp]]

   [value-of : [-> Exp Env ExpVal]]
   ))
