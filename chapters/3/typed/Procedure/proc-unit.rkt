#lang typed/racket

(require "../types/types.rkt"
         "../Expressions/exp-sig.rkt"
         "../Environment/env-sig.rkt"
         "proc-sig.rkt")

(provide proc@)


(define-unit proc@
  (import env^ exp^)
  (export proc^)

  (: proc? [-> Any Boolean : Proc])
  (define-predicate proc? Proc)

  (: procedure [-> (U Symbol (Listof Symbol)) Exp Env Proc])
  (define procedure
    (λ (vars body env)
      (make-proc vars
                 body
                 env
                 ;; (extend-env+ (free-vars (if (symbol? vars) (list vars) vars)
                 ;;                         body env)
                 ;;              (empty-env))
                 )))


  (: trace-proc? [-> Any Boolean : Trace-Proc])
  (define-predicate trace-proc? Trace-Proc)

  (: trace-procedure [-> (U Symbol (Listof Symbol)) Exp Env Trace-Proc])
  (define trace-procedure
    (λ (vars body env)
      (make-trace-proc vars
                       body
                       (extend-env+ (free-vars (if (symbol? vars) (list vars) vars)
                                               body env)
                                    (empty-env)))))

  (: apply-procedure [-> Proc (Listof DenVal) ExpVal])
  (define apply-procedure
    (λ (proc vals)
      (: vars (U Symbol (Listof Symbol)))
      (define vars (proc-vars proc))

      (: result ExpVal)
      (define result (symbol-val 'undefined))

      (dynamic-wind
        (λ ()
          (when (trace-proc? proc)
            (displayln (format "enter: ~a\n"
                               (if (symbol? vars)
                                   (cons vars vals)
                                   (map (ann (λ (var val) (cons var val))
                                             [-> Symbol DenVal (Pair Symbol DenVal)])
                                        vars vals))))))
        (λ ()
          (set! result
            (value-of (proc-body proc)
                      (if (symbol? vars)
                          (extend-env  vars
                                       vals
                                       (proc-saved-env proc))
                          (extend-env* vars
                                       vals
                                       (proc-saved-env proc))))))
        (λ ()
          (when (trace-proc? proc)
            (displayln (format "result: ~a\n" result)))))


      result))


  (: free-vars [-> (Listof Symbol) Exp Env (Listof (Pair Symbol DenVal))])
  (define free-vars
    (λ (vars exp env)
      (cond [(symbol-exp? exp) '()]
            [(const-exp? exp) '()]
            [(bool-exp? exp) '()]
            [(char-exp? exp) '()]
            [(string-exp? exp) '()]

            [(var-exp? exp)
             (let ([var : Symbol (var-exp-var exp)])
               (if (memq var vars)
                   '()
                   (list (cons var (apply-env env var)))))]

            [(begin-exp? exp)
             (let loop : (Listof (Pair Symbol DenVal))
                  ([exps : (Listof Exp) (begin-exp-exps exp)]
                   [bounds : (Listof (Pair Symbol DenVal)) '()])
               (if (null? exps)
                   bounds
                   (loop (cdr exps)
                         (append (free-vars (append (map (inst car Symbol DenVal) bounds) vars)
                                            (car exps)
                                            env)
                                 bounds))))]

            [(if-exp? exp)
             (free-vars vars
                        (begin-exp (list (if-exp-pred-exp  exp)
                                         (if-exp-true-exp  exp)
                                         (if-exp-false-exp exp)))
                        env)]
            [(cond-exp? exp)
             (free-vars vars
                        (begin-exp (apply append (cond-exp-exps exp)))
                        env)]
            [(let-exp? exp)
             (let loop : (Listof (Pair Symbol DenVal))
                  ([exps : (Listof Exp) (let-exp-bound-exps exp)]
                   [bounds : (Listof (Pair Symbol DenVal)) '()])
                  (if (null? exps)
                      (append (free-vars (append (map (inst car Symbol DenVal) bounds)
                                                 (let-exp-bound-vars exp)
                                                 vars)
                                         (let-exp-body exp)
                                         env)
                              bounds)
                      (loop (cdr exps)
                            (append (free-vars (append (map (inst car Symbol DenVal) bounds)
                                                       vars)
                                               (car exps)
                                               env)
                                    bounds))))]

            [(primitive-proc-exp? exp)
             (free-vars vars
                        (begin-exp (primitive-proc-exp-exps exp))
                        env)]
            [(proc-exp? exp)
             (let ([proc-vars (proc-exp-vars exp)])
               (free-vars (if (symbol? proc-vars)
                              (cons proc-vars vars)
                              (append proc-vars vars))
                          (proc-exp-body exp)
                          env))]
            [(call-exp? exp)
             (let ([rator (call-exp-rator exp)]
                   [rands (call-exp-rands exp)])
               (free-vars vars
                          (begin-exp (if (var-exp? rands)
                                         (list rator rands)
                                         (cons rator rands)))
                          env))]

            [else (raise-argument-error 'free-vars "exp?" exp)])))

  )
