#lang typed/racket

(provide (all-defined-out))


(define-struct env
  ([type : (U 'empty-env 'extend-env)]
   [has-binding? : [-> Symbol Boolean]]
   [apply-env : [-> Symbol Any]])
  #:transparent
  #:type-name Env)
