#lang typed/racket

(require "../types/types.rkt")

(provide cont^)


(define-signature cont^
  (
   [id-cont    : [-> Cont]]
   [end-cont   : [-> Cont]]
   [apply-cont : [-> Cont ExpVal FinalAnswer]]

   [assign-cont : [-> Symbol Env Cont Cont]]

   [begin-cont  : [-> (Pair Exp (Listof Exp)) Env Cont Cont]]
   [exps-cont   : [-> (Listof Exp) (Listof DenVal) Env Cont Cont]]

   [if-cont   : [-> Exp Exp Env Cont Cont]]
   [cond-cont : [-> Exp (Listof (List Exp Exp)) Env Cont Cont]]

   [let-cont    : [-> (Listof Exp) (Listof Symbol) Exp Env Cont Cont]]
   [letrec-cont : [-> Symbol (Listof Symbol) (Listof Exp) Exp Env Cont Cont]]

   [proc-cont : [-> Proc Cont Cont]]
   [primitive-proc-cont : [-> Symbol (Listof Exp) Env Cont Cont]]

   [rator-cont : [-> (U Var-Exp (Listof Exp)) Env Cont Cont]]
   [rands-cont : (case-> [-> Proc Cont Cont]
                         [-> Proc (Listof Exp) Env Cont Cont])]
   ))
