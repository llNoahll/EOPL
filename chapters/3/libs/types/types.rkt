#lang typed/racket

(provide (all-defined-out))


(define-type S-List (Listof S-Exp))
(define-type S-Exp (U Boolean Integer Symbol S-List))

(define-predicate s-exp?  S-Exp)
(define-predicate s-list? S-List)


(define-type DenVal (U Symbol Integer Boolean Null
                       (Pair DenVal DenVal)
                       Proc))
(define-type ExpVal (U DenVal Void Nothing))

(define-struct env
  ([type : (U 'empty-env 'extend-env)]
   [has-binding? : [-> Symbol Boolean]]
   [apply-env : [-> Symbol DenVal]])
  #:transparent
  #:type-name Env)


(define-struct proc
  ([vars : (U Symbol (Listof Symbol))]
   [body : Exp]
   [saved-env : Env])
  #:transparent
  #:type-name Proc)


(define-type Exp (U Symbol-Exp Const-Exp Bool-Exp If-Exp Cond-Exp
                    Primitive-Proc-Exp Proc-Exp Call-Exp
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

(define-struct if-exp
  ([pred-exp : Exp]
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

(define-struct let-exp
  ([bound-vars : (Listof Symbol)]
   [bound-exps : (Listof Exp)]
   [body : Exp])
  #:transparent
  #:type-name Let-Exp)


(define-struct primitive-proc-exp
  ([op : Symbol]
   [exps : (Listof Exp)])
  #:transparent
  #:type-name Primitive-Proc-Exp)

(define-struct proc-exp
  ([vars : (Listof Symbol)]
   [body : Exp])
  #:transparent
  #:type-name Proc-Exp)

(define-struct call-exp
  ([rator : Exp]
   [rands : (Listof Exp)])
  #:transparent
  #:type-name Call-Exp)
