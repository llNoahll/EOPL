#lang typed/racket

(require "../types/types.rkt")

(provide env^)


(define-signature env^
  (
   [base-env    : (Parameter Env)]
   [empty-env   : [-> Env]]
   [empty-env?  : [-> Env Boolean]]


   [extend-env  : [-> Symbol DenVal Env Env]]
   [extend-env* : [-> (Listof Symbol) (Listof DenVal) Env Env]]
   [extend-env+ : [-> (Listof (Pair Symbol DenVal)) Env Env]]

   [extend-env-bind  : [-> Symbol Ref Env Env]]
   [extend-env-bind* : [-> (Listof Symbol) (Listof Ref) Env Env]]
   [extend-env-bind+ : [-> (Listof (Pair Symbol Ref)) Env Env]]

   [extend-env? : [-> Env Boolean]]

   [env?          : [-> Any Boolean : Env]]
   [apply-env-ref : [-> Env Symbol Ref]]
   [apply-env     : [-> Env Symbol DenVal]]
   [has-binding?  : [-> Env Symbol Boolean]]
   [set-binding!  : [-> Env Symbol DenVal Void]]
   [copy-env      : [-> Env Env]]
   ))
