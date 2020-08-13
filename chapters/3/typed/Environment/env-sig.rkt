#lang typed/racket

(require "../types/types.rkt")

(provide env^)


(define-signature env^
  ([empty-env    : [-> Env]]
   [empty-env?   : [-> Env Boolean]]

   [extend-env   : [-> Symbol DenVal Env Env]]
   [extend-env*  : [-> (Listof Symbol)
                       (Listof DenVal)
                       Env
                       Env]]
   [extend-env+  : [-> (Listof (Pair Symbol DenVal)) Env Env]]
   [extend-env?  : [-> Env Boolean]]

   [env?         : [-> Any Boolean : Env]]
   [apply-env    : [-> Env Symbol DenVal]]
   [has-binding? : [-> Env Symbol Boolean]]))
