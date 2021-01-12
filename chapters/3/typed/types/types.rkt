#lang typed/racket

(require/typed racket/undefined
  [undefined Undefined])

(provide undefined (all-defined-out))


(define-predicate undefined? Undefined)
(define-predicate true? True)


(define-type Literal (U Boolean Real Symbol Char String))

(define-type S-List (Listof S-Exp))
(define-type S-Exp (U Literal S-List))

(define-predicate s-exp?  S-Exp)
(define-predicate s-list? S-List)

(define-type Lambda (U 'lambda 'λ))
(define-predicate λ? Lambda)
(define-predicate lambda? Lambda)

(define-type Trace-Lambda (U 'trace-lambda 'trace-λ))
(define-predicate trace-λ? Trace-Lambda)
(define-predicate trace-lambda? Trace-Lambda)


(define-type DenVal (U Literal Null Proc Trace-Proc
                       (Pair DenVal DenVal)))
(define-predicate denval? DenVal)

(define-type ExpVal (U DenVal Void Undefined Nothing))
(define-predicate expval? ExpVal)


(define-type Location (Parameter DenVal (U DenVal Undefined)))


(define-struct env
  ([type : (U 'empty-env 'extend-env 'extend-env-rec)]
   [has-binding? : [-> Symbol Boolean]]
   [apply-env : [-> Symbol Location]])
  #:transparent
  #:type-name Env)


(define-struct proc
  ([vars : (U Symbol (Listof Symbol))]
   [body : Exp]
   [saved-env : Env])
  #:transparent
  #:type-name Proc)

(define-struct (trace-proc proc)
  ()
  #:transparent
  #:type-name Trace-Proc)


(define-type Exp (U Symbol-Exp Const-Exp Bool-Exp Char-Exp String-Exp
                    If-Exp Cond-Exp
                    Begin-Exp Primitive-Proc-Exp Proc-Exp Trace-Proc-Exp Call-Exp
                    Var-Exp Let-Exp Letrec-Exp))
(define-predicate exp? Exp)


(define-struct symbol-exp ([symbol : Symbol])
  #:transparent
  #:type-name Symbol-Exp)

(define-struct const-exp ([num : Real])
  #:transparent
  #:type-name Const-Exp)

(define-struct bool-exp ([bool : Boolean])
  #:transparent
  #:type-name Bool-Exp)

(define-struct char-exp ([char : Char])
  #:transparent
  #:type-name Char-Exp)

(define-struct string-exp ([str : String])
  #:transparent
  #:type-name String-Exp)


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
  ([bind-vars : (Listof Symbol)]
   [bind-exps : (Listof Exp)]
   [body : Exp])
  #:transparent
  #:type-name Let-Exp)

(define-struct letrec-exp
  ([bind-vars : (Listof Symbol)]
   [bind-exps : (Listof Exp)]
   [body : Exp])
  #:transparent
  #:type-name Letrec-Exp)


(define-struct begin-exp ([exps : (Listof Exp)])
  #:transparent
  #:type-name Begin-Exp)


(define-struct primitive-proc-exp
  ([op : Symbol]
   [exps : (Listof Exp)])
  #:transparent
  #:type-name Primitive-Proc-Exp)

(define-struct proc-exp
  ([vars : (U Symbol (Listof Symbol))]
   [body : Exp])
  #:transparent
  #:type-name Proc-Exp)

(define-struct (trace-proc-exp proc-exp)
  ()
  #:transparent
  #:type-name Trace-Proc-Exp)

(define-struct call-exp
  ([rator : Exp]
   [rands : (U Var-Exp (Listof Exp))])
  #:transparent
  #:type-name Call-Exp)
