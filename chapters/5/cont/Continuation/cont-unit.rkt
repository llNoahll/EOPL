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
      (: make-cont [-> (Listof Exp) (Listof DenVal) Cont])
      (define make-cont
        (λ (exps vals)
          (λ (val)
            (if (null? exps)
                (apply-cont cont (reverse (cons (expval->denval val) vals)))
                (value-of/k (car exps)
                            env
                            (make-cont (cdr exps) (cons (expval->denval val) vals)))))))

      (make-cont exps vals)))


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
      (: make-cont [-> Exp (Listof (List Exp Exp)) Cont])
      (define make-cont
        (λ (body next)
          (λ (pred-val)
            (if (expval->bool pred-val)
                (value-of/k body env cont)
                (if (null? next)
                    (apply-cont cont (void))
                    (let ([branch (car next)])
                      (value-of/k (car branch)
                                  env
                                  (make-cont (cadr branch) (cdr next)))))))))

      (make-cont body next)))


  (: let-cont [-> (Listof Exp) (Listof Symbol) Exp Env Cont Cont])
  (define let-cont
    (λ (exps vars body env cont)
      (: make-cont [-> (Listof DenVal) (Listof Exp) Cont])
      (define make-cont
        (λ (vals exps)
          (λ (val)
            (if (null? exps)
                (value-of/k body
                            (extend-env* vars (reverse (cons (expval->denval val) vals)) env)
                            cont)
                (value-of/k (car exps) env
                            (make-cont (cons (expval->denval val) vals) (cdr exps)))))))

      (make-cont '() exps)))

  (: letrec-cont [-> Symbol (Listof Symbol) (Listof Exp) Exp Env Cont Cont])
  (define letrec-cont
    (λ (var vars exps body env cont)
      (: make-cont [-> Symbol (Listof Symbol) (Listof Exp) Cont])
      (define make-cont
        (λ (var vars exps)
          (λ (val)
            (set-binding! env var (expval->denval val))
            (if (or (null? vars) (null? exps))
                (value-of/k body env cont)
                (value-of/k (car exps) env
                            (make-cont (car vars) (cdr vars) (cdr exps)))))))

      (make-cont var vars exps)))

  (: primitive-proc-cont [-> Symbol (Listof Exp) Env Cont Cont])
  (define primitive-proc-cont
    (λ (op exps env cont)
      (: make-cont [-> (Listof DenVal) (Listof Exp) Cont])
      (define make-cont
        (λ (vals exps)
          (λ (val)
            (if (null? exps)
                (apply-cont cont (apply (hash-ref primitive-proc-table op)
                                        (reverse (cons (expval->denval val) vals))))
                (value-of/k (car exps)
                            env
                            (make-cont (cons (expval->denval val) vals) (cdr exps)))))))

      (make-cont '() exps)))


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
       (: make-cont [-> (Listof DenVal) (Listof Exp) Cont])
       (define make-cont
         (λ (vals rands)
           (λ (val)
             (if (null? rands)
                 (apply-procedure/k proc (reverse (cons (expval->denval val) vals)) cont)
                 (value-of/k (car rands)
                             env
                             (make-cont (cons (expval->denval val) vals) (cdr rands)))))))

       (make-cont '() rands)]))

  )
