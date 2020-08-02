#lang typed/racket

(require "../types/types.rkt")

(provide proc^)


(define-signature proc^
  (
   [proc? : [-> Any Boolean : Proc]]
   [procedure : [-> (U Symbol (Listof Symbol)) Exp Env Proc]]
   [apply-procedure : [-> Proc (Listof DenVal) ExpVal]]
   ))
