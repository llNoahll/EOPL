#lang typed/racket

(require "../types/types.rkt"
         "../Reference/ref-sig.rkt"
         "../Continuation/cont-sig.rkt"
         "../ExpValues/values-sig.rkt"
         "../Environment/env-sig.rkt"
         "../Procedure/proc-sig.rkt"
         "../PrimitiveProc/primitive-proc-sig.rkt"
         "exp-sig.rkt")

(provide exp@)


(define-unit exp@
  (import ref^ cont^ values^ env^ proc^ primitive-proc^)
  (export exp^)

  (: assign-exp [-> Symbol Exp Assign-Exp])
  (define assign-exp (λ (symbol exp) (make-assign-exp symbol exp)))


  (: symbol-exp [-> Symbol Symbol-Exp])
  (define symbol-exp (λ (symbol) (make-symbol-exp symbol)))

  (: const-exp [-> Real Const-Exp])
  (define const-exp (λ (num) (make-const-exp num)))

  (: bool-exp [-> Boolean Bool-Exp])
  (define bool-exp (λ (bool) (make-bool-exp bool)))

  (: char-exp [-> Char Char-Exp])
  (define char-exp (λ (ch) (make-char-exp ch)))

  (: string-exp [-> String String-Exp])
  (define string-exp (λ (str) (make-string-exp str)))


  (: if-exp [-> Exp Exp Exp If-Exp])
  (define if-exp
    (λ (pred-exp true-exp false-exp)
      (make-if-exp pred-exp true-exp false-exp)))

  (: cond-exp [-> (Pair (List Exp Exp) (Listof (List Exp Exp))) Cond-Exp])
  (define cond-exp
    (λ (exps)
      (make-cond-exp exps)))

  (: var-exp [-> Symbol Var-Exp])
  (define var-exp (λ (var) (make-var-exp var)))

  (: let-exp [-> (Listof Symbol) (Listof Exp) Exp Let-Exp])
  (define let-exp
    (λ (bind-vars bind-exps body)
      (make-let-exp bind-vars bind-exps body)))

  (: letrec-exp [-> (Listof Symbol) (Listof Exp) Exp Letrec-Exp])
  (define letrec-exp
    (λ (bind-vars bind-exps body)
      (make-letrec-exp bind-vars bind-exps body)))


  (: begin-exp [-> (Pair Exp (Listof Exp)) Begin-Exp])
  (define begin-exp
    (λ (exps)
      (make-begin-exp exps)))


  (: primitive-proc-exp [-> Symbol Exp * Primitive-Proc-Exp])
  (define primitive-proc-exp (λ (op . exps) (make-primitive-proc-exp op exps)))


  (: proc-exp [-> (U Symbol (Listof Symbol)) Exp Proc-Exp])
  (define proc-exp
    (λ (vars body)
      (make-proc-exp vars body)))

  (: trace-proc-exp [-> (U Symbol (Listof Symbol)) Exp Trace-Proc-Exp])
  (define trace-proc-exp
    (λ (vars body)
      (make-trace-proc-exp vars body)))

  (: call-exp [-> Exp (U Var-Exp (Listof Exp)) Call-Exp])
  (define call-exp
    (λ (rator rands)
      (make-call-exp rator rands)))


  (: value-of/k [-> Exp Env Cont FinalAnswer])
  (define value-of/k
    (λ (exp env cont)
      (cond [(assign-exp? exp)
             (value-of/k (assign-exp-exp exp)
                         env
                         (ann (λ (val)
                                (apply-cont cont
                                            (set-binding! env (assign-exp-var exp)
                                                          (expval->denval val))))
                              Cont))]

            [(symbol-exp? exp) (apply-cont cont (symbol-val (symbol-exp-symbol exp)))]
            [(const-exp? exp) (apply-cont cont (num-val (const-exp-num exp)))]
            [(bool-exp? exp) (apply-cont cont (bool-val (bool-exp-bool exp)))]
            [(char-exp? exp) (apply-cont cont (char-val (char-exp-char exp)))]
            [(string-exp? exp) (apply-cont cont (string-val (string-exp-str exp)))]
            [(var-exp? exp) (apply-cont cont (apply-env env (var-exp-var exp)))]

            [(begin-exp? exp)
             (let loop : FinalAnswer
                  ([exps : (Pair Exp (Listof Exp)) (begin-exp-exps exp)])
                  (define next (cdr exps))

                  (value-of/k (car exps)
                              env
                              (if (null? next)
                                  cont
                                  (ann (λ (val) (loop next)) Cont))))]

            [(if-exp? exp)
             (value-of/k (if-exp-pred-exp exp)
                         env
                         (ann (λ (pred-val)
                                (value-of/k (if (expval->bool pred-val)
                                                (if-exp-true-exp exp)
                                                (if-exp-false-exp exp))
                                            env
                                            cont))
                              Cont))]
            [(cond-exp? exp)
             (let loop : FinalAnswer
                  ([branches : (Listof (List Exp Exp)) (cond-exp-branches exp)])
                  (cond [(null? branches) (apply-cont cont (void))]
                        [else
                         (define branch (car branches))

                         (value-of/k (car branch)
                                     env
                                     (ann (λ (pred-val)
                                            (if (expval->bool pred-val)
                                                (value-of/k (cadr branch) env cont)
                                                (loop (cdr branches))))
                                          Cont))]))]
            [(let-exp? exp)
             (define vars (let-exp-bind-vars exp))
             (define body (let-exp-body exp))

             (let loop : FinalAnswer
                  ([exps (let-exp-bind-exps exp)]
                   [vals : (Listof DenVal) '()])
                  (if (null? exps)
                      (value-of/k body
                                  (extend-env* vars (reverse vals) env)
                                  cont)
                      (value-of/k (car exps)
                                  env
                                  (ann (λ (val)
                                         (loop (cdr exps)
                                               (cons (expval->denval val) vals)))
                                       Cont))))]
            [(letrec-exp? exp)
             (define vars (letrec-exp-bind-vars exp))
             (define exps (letrec-exp-bind-exps exp))
             (define body (letrec-exp-body exp))

             (define new-env
               (extend-env+ (map (ann (λ (var) (cons var undefined))
                                      [-> Symbol (Pair Symbol Undefined)])
                                 (letrec-exp-bind-vars exp))
                            env))

             (let loop : FinalAnswer
                  ([exps exps] [vars vars])
                  (if (null? exps)
                      (value-of/k body new-env cont)
                      (value-of/k (car exps)
                                  new-env
                                  (ann (λ (val)
                                         (set-binding! new-env (car vars) (expval->denval val))
                                         (loop (cdr exps) (cdr vars)))
                                       Cont))))]

            [(primitive-proc-exp? exp)
             (define op   (primitive-proc-exp-op   exp))
             (define exps (primitive-proc-exp-exps exp))

             (let loop : FinalAnswer
                  ([exps exps]
                   [vals : (Listof DenVal) '()])
                  (if (null? exps)
                      (apply-cont cont
                                  (apply (hash-ref primitive-proc-table op)
                                         (reverse vals)))
                      (value-of/k (car exps)
                                  env
                                  (ann (λ (val)
                                         (loop (cdr exps) (cons (expval->denval val) vals)))
                                       Cont))))]
            [(trace-proc-exp? exp)
             (apply-cont cont (proc-val (trace-procedure (proc-exp-vars exp) (proc-exp-body exp) env)))]
            [(proc-exp? exp)
             (apply-cont cont (proc-val (procedure (proc-exp-vars exp) (proc-exp-body exp) env)))]
            [(call-exp? exp)
             (define rator (call-exp-rator exp))
             (define rands (call-exp-rands exp))

             (value-of/k rator
                         env
                         (ann (λ (val)
                                (define proc (expval->proc val))

                                (if (var-exp? rands)
                                    (value-of/k rands
                                                env
                                                (ann (λ (args)
                                                       (apply-procedure/k proc (cast args (Listof DenVal)) cont))
                                                     Cont))
                                    (let loop : FinalAnswer
                                         ([rands rands] [args : (Listof DenVal) '()])
                                         (if (null? rands)
                                             (apply-procedure/k proc (reverse args) cont)
                                             (value-of/k (car rands)
                                                         env
                                                         (ann (λ (arg)
                                                                (loop (cdr rands)
                                                                      (cons (expval->denval arg) args)))
                                                              Cont))))))
                              Cont))]

            [else (raise-argument-error 'value-of "exp?" exp)])))

  )
