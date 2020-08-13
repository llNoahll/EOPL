#lang racket

(require (rename-in "../types/types.rkt" [proc? init-proc?])
         "../Expressions/exp-sig.rkt"
         "../Environment/env-sig.rkt"
         "proc-sig.rkt")

(provide proc@)


(define-unit proc@
  (import env^ exp^)
  (export proc^)

  (define proc? init-proc?)

  (define procedure
    (λ (vars body env)
      (make-proc vars
                 body
                 (extend-env+ (free-vars (if (symbol? vars) (list vars) vars)
                                         body env)
                              (empty-env)))))

  (define apply-procedure
    (λ (proc vals)
      (let ([vars (proc-vars proc)])
        (value-of (proc-body proc)
                  (if (symbol? vars)
                      (extend-env vars
                                  vals
                                  (proc-saved-env proc))
                      (extend-env* vars
                                   vals
                                   (proc-saved-env proc)))))))


  (define free-vars
    (λ (vars exp env)
      (cond [(symbol-exp? exp) '()]
            [(const-exp? exp) '()]
            [(bool-exp? exp) '()]
            [(char-exp? exp) '()]
            [(string-exp? exp) '()]

            [(var-exp? exp)
             (let ([var (var-exp-var exp)])
               (if (memq var vars)
                   '()
                   (list (cons var (apply-env env var)))))]

            [(begin-exp? exp)
             (let loop ([exps (begin-exp-exps exp)]
                        [bounds '()])
               (if (null? exps)
                   bounds
                   (loop (cdr exps)
                         (append (free-vars (append (map car bounds) vars)
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
             (let loop ([exps (let-exp-bound-exps exp)]
                        [bounds '()])
               (if (null? exps)
                   (append (free-vars (append (map car bounds)
                                              (let-exp-bound-vars exp)
                                              vars)
                                      (let-exp-body exp)
                                      env)
                           bounds)
                   (loop (cdr exps)
                         (append (free-vars (append (map car bounds)
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
