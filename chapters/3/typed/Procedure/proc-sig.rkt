#lang typed/racket

(require "../types/types.rkt")

(provide proc^)


(define-signature proc^
  (
   [proc? : [-> Any Boolean : Proc]]
   [procedure : [-> (U Symbol (Listof Symbol)) Exp Env Proc]]

   [trace-proc? : [-> Any Boolean : Trace-Proc]]
   [trace-procedure : [-> (U Symbol (Listof Symbol)) Exp Env Trace-Proc]]

   [apply-procedure : [-> Proc (Listof DenVal) ExpVal]]
   [free-bounds : [-> (Listof Symbol) Exp Env (Listof (Pair Symbol Location))]]
   ))
