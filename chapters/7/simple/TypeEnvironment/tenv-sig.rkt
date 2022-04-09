#lang typed/racket

(require "../types/types.rkt")

(provide tenv^)


(define-signature tenv^
  (
   [base-tenv    : (Parameter TEnv)]
   [empty-tenv   : [-> TEnv]]
   [empty-tenv?  : [-> TEnv Boolean]]


   [extend-tenv  : [-> Symbol Type TEnv TEnv]]
   [extend-tenv* : [-> (Listof Symbol) (Listof Type) TEnv TEnv]]
   [extend-tenv+ : [-> (Listof (Pair Symbol Type)) TEnv TEnv]]

   [extend-tenv? : [-> TEnv Boolean]]

   [tenv?          : [-> Any Boolean : TEnv]]
   [apply-tenv     : [-> TEnv Symbol Type]]
   [has-tbinding?  : [-> TEnv Symbol Boolean]]
   ))
