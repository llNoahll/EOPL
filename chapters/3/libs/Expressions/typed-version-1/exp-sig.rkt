#lang typed/racket

(require "../../types/version-1.rkt")

(provide exp^)


(define-signature exp^
  (
   [const-exp : [-> Integer Exp]]
   [zero?-exp : [-> Exp Exp]]
   [if-exp    : [-> Exp Exp Exp Exp]]
   [diff-exp  : [-> Exp Exp Exp]]
   [var-exp   : [-> Symbol Exp]]
   [let-exp   : [-> Symbol Exp Exp Exp]]

   [value-of : [-> Exp Env ExpVal]]
   ))
