#lang typed/racket


(module types typed/racket
  (require/typed racket/undefined
    [undefined Undefined])

  (require typed/racket/unsafe)

  (unsafe-require/typed
   racket/contract/base
   [or/c
    (All (A B C D E)
         (case-> [-> (pred Any)]
                 [-> (pred A) (pred A)]
                 [-> (pred A) (pred B) (pred (U A B))]
                 [-> (pred A) (pred B) (pred C) (pred (U A B C))]
                 [-> (pred A) (pred B) (pred C) (pred D) (pred (U A B C D))]
                 [-> (pred A) (pred B) (pred C) (pred D) (pred E) (pred (U A B C D E))]))]
   [and/c
    (All (A B C D E)
         (case-> [-> (pred Nothing)]
                 [-> (pred A) (pred A)]
                 [-> (pred A) (pred B) (pred (∩ A B))]
                 [-> (pred A) (pred B) (pred C) (pred (∩ A B C))]
                 [-> (pred A) (pred B) (pred C) (pred D) (pred (∩ A B C D))]
                 [-> (pred A) (pred B) (pred C) (pred D) (pred E) (pred (∩ A B C D E))]))])

  (provide
   undefined
   or/c and/c
   (rename-out
    #;[trace-lambda   trace-λ]
    [case-lambda    case-λ]
    [match-lambda   match-λ]
    [match-lambda*  match-λ*]
    [match-lambda** match-λ**]

    [add1 1+]
    [sub1 1-]
    [sub1 -1+])
   (all-defined-out))


  (define-predicate undefined? Undefined)
  (define-predicate true? True)

  (define-type Literal (U Boolean Real Char String))
  (define-predicate literal? Literal)


  (define-type S-Exp  (U Literal Symbol S-List))
  (define-predicate s-exp?  S-Exp)
  (define-type S-List (Listof S-Exp))
  (define-predicate s-list? S-List)

  (define-type Lambda (U 'lambda 'λ))
  (define-predicate λ? Lambda)
  (define-predicate lambda? Lambda)

  (define-type Trace-Lambda (U 'trace-lambda 'trace-λ))
  (define-predicate trace-λ? Trace-Lambda)
  (define-predicate trace-lambda? Trace-Lambda)


  (: listof? (All (A) (case-> [-> (pred A) (pred (Listof A))]
                              [-> [-> Any Boolean] [-> Any Boolean]])))
  (define listof?
    (λ (pred)
      (λ (arg)
        (and (list? arg)
             (andmap pred arg)))))

  (: empty-list [-> Null])
  (define empty-list (λ () '()))

  (define-type (Queueof A) (List (Listof A) (Listof A)))
  (define-predicate queue? (Queueof Any))
  (define-predicate empty-queue? (Queueof Nothing))

  (: queueof? (All (A) (case-> [-> (pred A) (pred (Queueof A))]
                               [-> [-> Any Boolean] [-> Any Boolean]])))
  (define queueof?
    (λ (pred)
      (λ (arg)
        (and (queue? arg)
             ((listof? pred) (car  arg))
             ((listof? pred) (cadr arg))))))

  (: empty-queue [-> (Queueof Nothing)])
  (define empty-queue (let ([empty-q '(() ())]) (λ () empty-q)))

  (: enqueue (All (A) [-> (Queueof A) A (Queueof A)]))
  (define enqueue
    (λ (q arg)
      (match q
        [`(,in ,out)
         `((,arg . ,in) ,out)])))

  (: dequeue (All (A B) [-> (Queueof A) [-> A (Queueof A) B] B]))
  (define dequeue
    (λ (q f)
      (match q
        ['(() ())
         (raise-argument-error 'dequeue "non-empty-queue?" q)]
        [`(,in ())
         (define l (reverse in))
         (f (car l) `(() ,(cdr l)))]
        [`(,in (,1st . ,out))
         (f 1st `(,in ,out))])))


  (define-type Cont (Listof Frame))
  (define-predicate cont? Cont)
  (struct frame
    ([type : Symbol]
     [func : [-> Cont [-> ExpVal FinalAnswer]]])
    #:type-name Frame)


  (define-type DenVal (U Literal Symbol Undefined Void Null
                         Primitive-Proc Proc Trace-Proc
                         (Queueof DenVal)

                         (Boxof     DenVal)
                         (Pairof    DenVal DenVal)
                         #;(MPairof   DenVal DenVal)
                         (Vectorof  DenVal)
                         (HashTable DenVal DenVal)))
  (define-type ExpVal DenVal)
  (define-new-subtype FinalAnswer (final-answer ExpVal))


  (define-struct ref
    ([val : DenVal])
    #:mutable
    #:type-name Ref)


  (define-struct env
    ([type  : (U 'empty-env 'extend-env)]
     [binds : (Immutable-HashTable Symbol Ref)])
    #:type-name Env)


  (define-struct primitive-proc
    ([λ : [-> DenVal * ExpVal]])
    #:type-name Primitive-Proc)

  (define-struct proc
    ([vars : (U Symbol (Listof Symbol))]  ; Symbol is used for `apply-primitive'.
     [body : Exp]
     [saved-env : Env])
    #:type-name Proc)

  (define-struct (trace-proc proc) () #:type-name Trace-Proc)


  (: denval? [-> Any Boolean])
  (define denval?
    (λ (arg)
      (or (literal? arg)
          (symbol? arg)
          (undefined? arg)
          (void? arg)
          (null? arg)
          (primitive-proc? arg)
          (proc? arg)
          (trace-proc? arg)
          ((queueof? denval?) arg)

          (denbox? arg)
          (denpair? arg)
          #;(denmpair? arg)
          (denvector? arg)
          (denhash? arg))))

  (: denbox? [-> Any Boolean])
  (define denbox? (λ (arg) (and (box? arg) (denval? (unbox arg)))))

  (: denpair? [-> Any Boolean])
  (define denpair? (λ (arg) (and (pair? arg) (denval? (car arg)) (denval? (cdr arg)))))

  #;(: denmpair? [-> Any Boolean])
  #;(define denmpair? (λ (arg) (and (mpair? arg) (denval? (mcar arg)) (denval? (mcdr arg)))))

  (: denvector? [-> Any Boolean])
  (define denvector? (λ (arg) (and (vector? arg) (for/and ([i (in-vector arg)]) (denval? i)))))

  (: denmvector? [-> Any Boolean])
  (define denmvector? (λ (arg) (and (denvector? arg) (not (immutable? arg)))))

  (: denimvector? [-> Any Boolean])
  (define denimvector? (λ (arg) (and (denvector? arg) (immutable? arg))))

  (: denhash? [-> Any Boolean])
  (define denhash? (λ (arg) (and (hash? arg) (for/and ([(k v) (in-hash arg)]) (and (denval? k) (denval? v))))))

  (: denmhash? [-> Any Boolean])
  (define denmhash? (λ (arg) (and (denhash? arg) (not (immutable? arg)))))

  (: denimhash? [-> Any Boolean])
  (define denimhash? (λ (arg) (and (denhash? arg) (not (immutable? arg)))))


  (: expval? [-> Any Boolean])
  (define expval? (λ (arg) (denval? arg)))

  (: final-answer? [-> Any Boolean])
  (define final-answer? (λ (arg) (expval? arg)))


  (define-struct exp () #:transparent #:type-name Exp)


  (define-struct (assign-exp exp)
    ([var : Symbol]
     [exp : Exp])
    #:transparent
    #:type-name Assign-Exp)


  (define-struct (quote-exp exp)
    ([datum : S-Exp])
    #:transparent
    #:type-name Quote-Exp)

  (define-struct (symbol-exp exp)
    ([symbol : Symbol])
    #:transparent
    #:type-name Symbol-Exp)

  (define-struct (real-exp exp)
    ([num : Real])
    #:transparent
    #:type-name Real-Exp)

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
    ([pred-exp  : Exp]
     [true-exp  : Exp]
     [false-exp : Exp])
    #:transparent
    #:type-name If-Exp)


  (define-struct (var-exp exp)
    ([var : Symbol])
    #:transparent
    #:type-name Var-Exp)


  (define-struct (begin-exp exp)
    ([exps : (Pair Exp (Listof Exp))])
    #:transparent
    #:type-name Begin-Exp)


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
     [rands : (U Var-Exp (Listof Exp))])  ; Symbol is used for `apply'.
    #:transparent
    #:type-name Call-Exp)

  )

(require (except-in 'types
                    denval?
                    denbox?
                    denpair?
                    #;denmpair?
                    denvector?
                    denmvector?
                    denimvector?
                    denhash?
                    denmhash?
                    denimhash?

                    expval?
                    final-answer?))
(provide (all-from-out 'types))

(require typed/racket/unsafe)
(unsafe-require/typed/provide
 'types
 [denval?       (pred DenVal)]
 [denbox?       (pred (Boxof DenVal))]
 [denpair?      (pred (Pairof DenVal DenVal))]
 #;[denmpair?     (pred (MPairof DenVal DenVal))]
 [denvector?    (pred (Vectorof DenVal))]
 [denmvector?   (pred (Mutable-Vectorof DenVal))]
 [denimvector?  (pred (Immutable-Vectorof DenVal))]
 [denhash?      (pred (HashTable DenVal DenVal))]
 [denmhash?     (pred (Mutable-HashTable DenVal DenVal))]
 [denimhash?    (pred (Immutable-HashTable DenVal DenVal))]

 [expval?       (pred ExpVal)]
 [final-answer? (pred FinalAnswer)])
