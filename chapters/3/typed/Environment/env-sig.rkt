#lang typed/racket

(require "../types/types.rkt")

(provide env^)


(define-signature env^
  ([empty-env   : [-> Env]]
   [empty-env?  : [-> Env Boolean]]

   [extend-env  : [-> Symbol DenVal Env Env]]
   [extend-env* : [-> (Listof Symbol) (Listof DenVal) Env Env]]
   [extend-env+ : [-> (Listof (Pair Symbol DenVal)) Env Env]]

   [extend-env-bound  : [-> Symbol Location Env Env]]
   [extend-env-bound* : [-> (Listof Symbol) (Listof Location) Env Env]]
   [extend-env-bound+ : [-> (Listof (Pair Symbol Location)) Env Env]]

   [extend-env? : [-> Env Boolean]]


   [extend-env-rec  : [-> Symbol Exp Env Env]]
   [extend-env-rec* : [-> (Listof Symbol) (Listof Exp) Env Env]]
   [extend-env-rec+ : [-> (Listof (Pair Symbol Exp)) Env Env]]

   [extend-env-rec-bound  : [-> Symbol Location Env Env]]
   [extend-env-rec-bound* : [-> (Listof Symbol) (Listof Location) Env Env]]
   [extend-env-rec-bound+ : [-> (Listof (Pair Symbol Location)) Env Env]]

   [extend-env-rec? : [-> Env Boolean]]

   [env?         : [-> Any Boolean : Env]]
   [apply-env    : [-> Env Symbol Location]]
   [has-binding? : [-> Env Symbol Boolean]]))
