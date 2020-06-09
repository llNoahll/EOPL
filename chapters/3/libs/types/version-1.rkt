#lang typed/racket

(provide (all-defined-out))


(define-type ExpVal (U Integer Boolean))
(define-type DenVal (U Integer Boolean))

(define-struct env
  ([type : (U 'empty-env 'extend-env)]
   [has-binding? : [-> Symbol Boolean]]
   [apply-env : [-> Symbol Any]])
  #:type-name Env)


(define-type Exp (U Const-Exp Zero?-Exp If-Exp Diff-Exp Var-Exp Let-Exp))
(define-predicate exp? Exp)

(define-struct const-exp ([num : Integer]) #:type-name Const-Exp)
(define-struct zero?-exp ([exp : Exp]) #:type-name Zero?-Exp)
(define-struct if-exp ([pred-exp : Exp]
                       [true-exp : Exp]
                       [false-exp : Exp])
  #:type-name If-Exp)
(define-struct diff-exp ([minuend-exp : Exp]
                         [subtrahend-exp : Exp])
  #:type-name Diff-Exp)
(define-struct var-exp ([var : Symbol]) #:type-name Var-Exp)
(define-struct let-exp ([bound-var : Symbol]
                        [bound-exp : Exp]
                        [body : Exp])
  #:type-name Let-Exp)
