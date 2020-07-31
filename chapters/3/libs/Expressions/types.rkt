#lang typed/racket

(provide (all-defined-out))


(define-type Exp (U Symbol-Exp Const-Exp Bool-Exp If-Exp Cond-Exp
                    Nullary-Exp Unary-Exp Binary-Exp N-ary-Exp
                    Var-Exp Let-Exp))
(define-predicate exp? Exp)


(define-struct symbol-exp ([symbol : Symbol])
  #:transparent
  #:type-name Symbol-Exp)

(define-struct const-exp ([num : Integer])
  #:transparent
  #:type-name Const-Exp)

(define-struct bool-exp ([bool : Boolean])
  #:transparent
  #:type-name Bool-Exp)

(define-struct if-exp ([pred-exp : Exp]
                       [true-exp : Exp]
                       [false-exp : Exp])
  #:transparent
  #:type-name If-Exp)

(define-struct cond-exp ([exps : (Listof (Pair Exp (Listof Exp)))])
  #:transparent
  #:type-name Cond-Exp)


(define-struct var-exp ([var : Symbol])
  #:transparent
  #:type-name Var-Exp)

(define-struct let-exp ([bound-vars : (Listof Symbol)]
                        [bound-exps : (Listof Exp)]
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
