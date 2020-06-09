#lang typed/racket

(require "../../types/version-1.rkt")

(provide env^)


(define-signature env^
  ([empty-env    : [-> Env]]
   [empty-env?   : [-> Env Boolean]]
   [extend-env   : [-> Symbol Any Env Env]]
   [extend-env*  : [-> (Pair Symbol (Listof Symbol))
                       (Pair Any (Listof Any))
                       Env
                       Env]]
   [extend-env?  : [-> Env Boolean]]
   [env?         : [-> Any Boolean : Env]]
   [apply-env    : [-> Env Symbol Any]]
   [has-binding? : [-> Env Symbol Boolean]]))
