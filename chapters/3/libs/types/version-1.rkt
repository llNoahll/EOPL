#lang typed/racket

(provide (all-defined-out))


(define-type DenVal (U Integer Boolean (Pair DenVal DenVal) Null))
(define-type ExpVal (U DenVal Void Nothing))

(define-struct env
  ([type : (U 'empty-env 'extend-env)]
   [has-binding? : [-> Symbol Boolean]]
   [apply-env : [-> Symbol Any]])
  #:type-name Env)


(define-type Exp (U Const-Exp If-Exp
                    Nullary-Exp Unary-Exp Binary-Exp N-ary-Exp
                    Var-Exp Let-Exp))
(define-predicate exp? Exp)


(define-struct const-exp ([num : Integer]) #:type-name Const-Exp)

(define-struct if-exp ([pred-exp : Exp]
                       [true-exp : Exp]
                       [false-exp : Exp])
  #:type-name If-Exp)

(define-struct var-exp ([var : Symbol]) #:type-name Var-Exp)

(define-struct let-exp ([bound-var : Symbol]
                        [bound-exp : Exp]
                        [body : Exp])
  #:type-name Let-Exp)

(define-struct nullary-exp ([op : Symbol]) #:type-name Nullary-Exp)
(define-struct unary-exp   ([op : Symbol] [exp : Exp]) #:type-name Unary-Exp)
(define-struct binary-exp  ([op : Symbol] [exp-1 : Exp] [exp-2 : Exp]) #:type-name Binary-Exp)
(define-struct n-ary-exp   ([op : Symbol] [exps : (Listof Exp)]) #:type-name N-ary-Exp)
