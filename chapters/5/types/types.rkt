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


(define-struct ref
  ([val : (Boxof DenVal)])
  #:transparent
  #:type-name Ref)


(define-type DenVal (U Literal Undefined
                       Proc Trace-Proc
                       Null (Pair DenVal DenVal)))
(define-predicate denval? DenVal)

(define-type ExpVal (U DenVal Void Nothing))
(define-predicate expval? ExpVal)

(define-type FinalAnswer ExpVal)
(define-predicate final-answer? FinalAnswer)

(define-type Cont [-> ExpVal FinalAnswer])


(define-struct env
  ([type : (U 'empty-env 'extend-env 'extend-env-rec)]
   [has-binding? : [-> Symbol Boolean]]
   [apply-env-ref : [-> Symbol Ref]])
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


(define-struct exp ()
  #:transparent
  #:type-name Exp)


(define-struct (assign-exp exp)
  ([var : Symbol]
   [exp : Exp])
  #:transparent
  #:type-name Assign-Exp)


(define-struct (symbol-exp exp)
  ([symbol : Symbol])
  #:transparent
  #:type-name Symbol-Exp)

(define-struct (const-exp exp)
  ([num : Real])
  #:transparent
  #:type-name Const-Exp)

(define-struct (bool-exp exp)
  ([bool : Boolean])
  #:transparent
  #:type-name Bool-Exp)

(define-struct (char-exp exp)
  ([char : Char])
  #:transparent
  #:type-name Char-Exp)

(define-struct (string-exp exp)
  ([str : String])
  #:transparent
  #:type-name String-Exp)


(define-struct (if-exp exp)
  ([pred-exp : Exp]
   [true-exp : Exp]
   [false-exp : Exp])
  #:transparent
  #:type-name If-Exp)

(define-struct (cond-exp exp)
  ([branches : (Pair (List Exp Exp) (Listof (List Exp Exp)))])
  #:transparent
  #:type-name Cond-Exp)


(define-struct (var-exp exp)
  ([var : Symbol])
  #:transparent
  #:type-name Var-Exp)

(define-struct (let-exp exp)
  ([bind-vars : (Listof Symbol)]
   [bind-exps : (Listof Exp)]
   [body : Exp])
  #:transparent
  #:type-name Let-Exp)

(define-struct (letrec-exp exp)
  ([bind-vars : (Listof Symbol)]
   [bind-exps : (Listof Exp)]
   [body : Exp])
  #:transparent
  #:type-name Letrec-Exp)


(define-struct (begin-exp exp)
  ([exps : (Pair Exp (Listof Exp))])
  #:transparent
  #:type-name Begin-Exp)


(define-struct (primitive-proc-exp exp)
  ([op : Symbol]
   [exps : (Listof Exp)])
  #:transparent
  #:type-name Primitive-Proc-Exp)

(define-struct (proc-exp exp)
  ([vars : (U Symbol (Listof Symbol))]
   [body : Exp])
  #:transparent
  #:type-name Proc-Exp)

(define-struct (trace-proc-exp proc-exp)
  ()
  #:transparent
  #:type-name Trace-Proc-Exp)

(define-struct (call-exp exp)
  ([rator : Exp]
   [rands : (U Var-Exp (Listof Exp))])
  #:transparent
  #:type-name Call-Exp)
