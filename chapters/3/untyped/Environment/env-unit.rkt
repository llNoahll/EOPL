#lang racket

(require (rename-in "../types/types.rkt" [env? init-env?])
         "env-sig.rkt")

(provide env@)


(define-unit env@
  (import)
  (export env^)


  (define report-no-binding-found
    (λ (search-var)
      (error 'apply-env "No binding for ~s" search-var)))

  (define report-invalid-env
    (λ (env)
      (error 'apply-env "Bad environment: ~s" env)))


  (define empty-env
    (let ([empty-environment
           (make-env 'empty-env
                     (λ (search-var) #f)
                     (λ (search-var) (report-no-binding-found search-var)))])
      (λ ()
        empty-environment)))

  (define empty-env? (λ (env) (eqv? (env-type env) 'empty-env)))


  (define extend-env
    (λ (var val env)
      (make-env 'extend-env
                (λ (search-var)
                  (if (eqv? var search-var)
                      #t
                      (has-binding? env search-var)))
                (λ (search-var)
                  (if (eqv? search-var var)
                      val
                      (apply-env env search-var))))))


  (define extend-env*
    (λ (vars vals env)
      (cond [(and (null? vars) (null? vals)) env]
            [(= (length vars) (length vals))
             (make-env 'extend-env
                       (λ (search-var)
                         (define exist-var?
                           (λ (vars vals)
                             (cond [(or (null? vars) (null? vals)) #f]
                                   [(eqv? search-var (car vars)) #t]
                                   [else (exist-var? (cdr vars) (cdr vals))])))

                         (if (exist-var? vars vals)
                             #t
                             (has-binding? env search-var)))
                       (λ (search-var)
                         (define look-for-val
                           (λ (vars vals)
                             (cond [(null? vars) (apply-env env search-var)]
                                   [(eqv? search-var (car vars)) (car vals)]
                                   [else (look-for-val (cdr vars) (cdr vals))])))

                         (look-for-val vars vals)))]
            [else (error 'extend-env* "Bad input! vars: ~s, vals: ~s" vars vals)])))

  (define extend-env+
    (λ (bounds env)
      (extend-env* (map (λ (bound)
                          (car bound))
                        bounds)
                   (map (λ (bound)
                          (cdr bound))
                        bounds)
                   env)))


  (define extend-env? (λ (env) (eqv? (env-type env) 'extend-env)))


  (define env? init-env?)


  (define apply-env
    (λ (env search-var)
      ((env-apply-env env) search-var)))


  (define has-binding?
    (λ (env search-var)
      ((env-has-binding? env) search-var))))
