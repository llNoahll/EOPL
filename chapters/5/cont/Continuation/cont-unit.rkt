#lang typed/racket

(require "../types/types.rkt"
         "../Expressions/exp-sig.rkt"
         "../ExpValues/values-sig.rkt"
         "../Environment/env-sig.rkt"
         "../Procedure/proc-sig.rkt"
         "../PrimitiveProc/primitive-proc-sig.rkt"
         "cont-sig.rkt")

(provide cont@)


(define-unit cont@
  (import exp^ values^ env^ proc^ primitive-proc^)
  (export cont^)

  (: id-cont [-> Cont])
  (define id-cont (λ () (λ (val) (final-answer val))))

  (: end-cont [-> Cont])
  (define end-cont
    (λ ()
      (displayln "End of Computation!")
      (id-cont)))

  (: apply-cont [-> Cont ExpVal FinalAnswer])
  (define apply-cont
    (λ (cont val)
      (cont val)))


  (: assign-cont [-> Symbol Env Cont Cont])
  (define assign-cont
    (λ (var env cont)
      (λ (val)
        (apply-cont cont (set-binding! env var (expval->denval val))))))


  (: begin-cont [-> (Pair Exp (Listof Exp)) Env Cont Cont])
  (define begin-cont
    (λ (exps env cont)
      (λ (val)
        (value-of/k (begin-exp exps) env cont))))

  (: exps-cont [-> (Listof Exp) (Listof DenVal) Env Cont Cont])
  (define exps-cont
    (λ (exps vals env cont)
      (let loop : Cont ([exps exps] [vals vals])
           (λ (val)
             (if (null? exps)
                 (apply-cont cont (reverse (cons (expval->denval val) vals)))
                 (value-of/k (car exps)
                             env
                             (loop (cdr exps) (cons (expval->denval val) vals))))))))


  (: if-cont [-> Exp Exp Env Cont Cont])
  (define if-cont
    (λ (true-exp false-exp env cont)
      (λ (pred-val)
        (value-of/k (if (expval->bool pred-val)
                        true-exp
                        false-exp)
                    env
                    cont))))

  (: cond-cont [-> Exp (Listof (List Exp Exp)) Env Cont Cont])
  (define cond-cont
    (λ (body next env cont)
      (let loop : Cont ([body body] [next next])
           (λ (pred-val)
             (if (expval->bool pred-val)
                 (value-of/k body env cont)
                 (if (null? next)
                     (apply-cont cont (void))
                     (let ([branch (car next)])
                       (value-of/k (car branch)
                                   env
                                   (loop (cadr branch) (cdr next))))))))))


  (: let-cont [-> (Listof Exp) (Listof Symbol) Exp Env Cont Cont])
  (define let-cont
    (λ (exps vars body env cont)
      (let loop : Cont ([vals '()] [exps exps])
           (λ (val)
             (if (null? exps)
                 (value-of/k body
                             (extend-env* vars (reverse (cons (expval->denval val) vals)) env)
                             cont)
                 (value-of/k (car exps) env
                             (loop (cons (expval->denval val) vals) (cdr exps))))))))

  (: letrec-cont [-> Symbol (Listof Symbol) (Listof Exp) Exp Env Cont Cont])
  (define letrec-cont
    (λ (var vars exps body env cont)
      (let loop : Cont ([var var] [vars vars] [exps exps])
           (λ (val)
             (set-binding! env var (expval->denval val))
             (if (or (null? vars) (null? exps))
                 (value-of/k body env cont)
                 (value-of/k (car exps) env
                             (loop (car vars) (cdr vars) (cdr exps))))))))


  (: proc-cont [-> Proc Cont Cont])
  (define proc-cont
    (λ (proc cont)
      (λ (val)
        (when (trace-proc? proc)
          (displayln (format "result: ~a\n" val)))
        (cont val))))

  (: primitive-proc-cont [-> Symbol (Listof Exp) Env Cont Cont])
  (define primitive-proc-cont
    (λ (op exps env cont)
      (let loop : Cont ([vals '()] [exps exps])
           (λ (val)
             (if (null? exps)
                 (apply-cont cont (apply (hash-ref primitive-proc-table op)
                                         (reverse (cons (expval->denval val) vals))))
                 (value-of/k (car exps)
                             env
                             (loop (cons (expval->denval val) vals) (cdr exps))))))))


  (: rator-cont [-> (U Var-Exp (Listof Exp)) Env Cont Cont])
  (define rator-cont
    (λ (rands env cont)
      (λ (val)
        (define proc (expval->proc val))

        (cond [(var-exp? rands)
               (value-of/k rands
                           env
                           (rands-cont proc cont))]
              [(null? rands)
               (apply-procedure/k proc '() cont)]
              [else
               (value-of/k (car rands)
                           env
                           (rands-cont proc (cdr rands) env cont))]))))

  (: rands-cont (case-> [-> Proc Cont Cont]
                        [-> Proc (Listof Exp) Env Cont Cont]))
  (define rands-cont
    (case-lambda
      [(proc cont)
       (λ (vals)
         (apply-procedure/k proc (cast vals (Listof DenVal)) cont))]
      [(proc rands env cont)
       (let loop : Cont ([vals '()] [rands rands])
            (λ (val)
              (if (null? rands)
                  (apply-procedure/k proc (reverse (cons (expval->denval val) vals)) cont)
                  (value-of/k (car rands)
                              env
                              (loop (cons (expval->denval val) vals) (cdr rands))))))]))

  )
