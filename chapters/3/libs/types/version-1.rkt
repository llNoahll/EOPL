#lang typed/racket

(provide (all-defined-out))


(define-type DenVal (U Integer Boolean (Pair DenVal DenVal) Null))
(define-type ExpVal (U DenVal Void Nothing))

(define-struct env
  ([type : (U 'empty-env 'extend-env)]
   [has-binding? : [-> Symbol Boolean]]
   [apply-env : [-> Symbol Any]])
  #:transparent
  #:type-name Env)


(define-type Exp (U Const-Exp If-Exp
                    Nullary-Exp Unary-Exp Binary-Exp N-ary-Exp
                    Var-Exp Let-Exp))
(define-predicate exp? Exp)


(define-struct const-exp ([num : Integer])
  #:transparent
  #:type-name Const-Exp)

(define-struct if-exp ([pred-exp : Exp]
                       [true-exp : Exp]
                       [false-exp : Exp])
  #:transparent
  #:type-name If-Exp)

(define-struct var-exp ([var : Symbol])
  #:transparent
  #:type-name Var-Exp)

(define-struct let-exp ([bound-var : Symbol]
                        [bound-exp : Exp]
                        [body : Exp])
  #:transparent
  #:type-name Let-Exp)


(define-struct nullary-exp ([op : Symbol])
  #:transparent
  #:type-name Nullary-Exp)

(define-struct unary-exp   ([op : Symbol] [exp : Exp])
  #:transparent
  #:type-name Unary-Exp)

(define-struct binary-exp  ([op : Symbol] [exp-1 : Exp] [exp-2 : Exp])
  #:transparent
  #:type-name Binary-Exp)

(define-struct n-ary-exp   ([op : Symbol] [exps : (Listof Exp)])
  #:transparent
  #:type-name N-ary-Exp)
