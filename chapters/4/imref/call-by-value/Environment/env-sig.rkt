#lang typed/racket

(require "../types/types.rkt")

(provide env^)


(define-signature env^
  ([empty-env   : [-> Env]]
   [empty-env?  : [-> Env Boolean]]


   [extend-env  : [-> Symbol DenVal Env Env]]
   [extend-env* : [-> (Listof Symbol) (Listof DenVal) Env Env]]
   [extend-env+ : [-> (Listof (Pair Symbol DenVal)) Env Env]]

   [extend-env-bind  : [-> Symbol Ref Env Env]]
   [extend-env-bind* : [-> (Listof Symbol) (Listof Ref) Env Env]]
   [extend-env-bind+ : [-> (Listof (Pair Symbol Ref)) Env Env]]

   [extend-env? : [-> Env Boolean]]


   [extend-env-rec  : [-> Symbol Exp Env Env]]
   [extend-env-rec* : [-> (Listof Symbol) (Listof Exp) Env Env]]
   [extend-env-rec+ : [-> (Listof (Pair Symbol Exp)) Env Env]]

   [extend-env-rec-bind  : [-> Symbol Ref Env Env]]
   [extend-env-rec-bind* : [-> (Listof Symbol) (Listof Ref) Env Env]]
   [extend-env-rec-bind+ : [-> (Listof (Pair Symbol Ref)) Env Env]]

   [extend-env-rec? : [-> Env Boolean]]


   [env?          : [-> Any Boolean : Env]]
   [apply-env-ref : [-> Env Symbol Ref]]
   [apply-env     : [-> Env Symbol DenVal]]
   [has-binding?  : [-> Env Symbol Boolean]]
   [set-binding!  : [-> Env Symbol DenVal Void]]
   ))
