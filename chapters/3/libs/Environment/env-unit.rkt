#lang typed/racket

(require "../types/types.rkt"
         "env-sig.rkt")

(provide env@)


(define-unit env@
  (import)
  (export env^)


  (: report-no-binding-found [-> Symbol Nothing])
  (define report-no-binding-found
    (λ (search-var)
      (error 'apply-env "No binding for ~s" search-var)))

  (: report-invalid-env [-> Env Nothing])
  (define report-invalid-env
    (λ (env)
      (error 'apply-env "Bad environment: ~s" env)))


  (: empty-env [-> Env])
  (define empty-env
    (let ([empty-environment
           (make-env 'empty-env
                     (λ ([search-var : Symbol]) : Boolean #f)
                     (λ ([search-var : Symbol]) : Nothing (report-no-binding-found search-var)))])
      (λ ()
        empty-environment)))

  (: empty-env? [-> Env Boolean])
  (define empty-env? (λ (env) (eqv? (env-type env) 'empty-env)))


  (: extend-env [-> Symbol Any Env Env])
  (define extend-env
    (λ (var val env)
      (make-env 'extend-env
                (λ ([search-var : Symbol]) : Boolean
                    (if (eqv? var search-var)
                        #t
                        (has-binding? env search-var)))
                (λ ([search-var : Symbol]) : Any
                    (if (eqv? search-var var)
                        val
                        (apply-env env search-var))))))

  (: extend-env* [-> (Listof Symbol)
                     (Listof Any)
                     Env
                     Env])
  (define extend-env*
    (λ (vars vals env)
      (cond [(and (null? vars) (null? vals)) env]
            [(= (length vars) (length vals))
             (make-env 'extend-env
                       (λ ([search-var : Symbol]) : Boolean
                           (: exist-var? [-> (Listof Symbol) (Listof Any) Boolean])
                           (define exist-var?
                             (λ (vars vals)
                               (cond [(or (null? vars) (null? vals)) #f]
                                     [(eqv? search-var (car vars)) #t]
                                     [else (exist-var? (cdr vars) (cdr vals))])))

                           (if (exist-var? vars vals)
                               #t
                               (has-binding? env search-var)))
                       (λ ([search-var : Symbol]) : Any
                           (: look-for-val [-> (Listof Symbol) (Listof Any) Any])
                           (define look-for-val
                             (λ (vars vals)
                               (cond [(null? vars) (apply-env env search-var)]
                                     [(eqv? search-var (car vars)) (car vals)]
                                     [else (look-for-val (cdr vars) (cdr vals))])))

                           (look-for-val vars vals)))]
            [else (error 'extend-env* "Bad input! vars: ~s, vals: ~s" vars vals)])))


  (: extend-env? [-> Env Boolean])
  (define extend-env? (λ (env) (eqv? (env-type env) 'extend-env)))


  (: env? [-> Any Boolean : Env])
  (define-predicate env? Env)


  (: apply-env [-> Env Symbol Any])
  (define apply-env
    (λ (env search-var)
      ((env-apply-env env) search-var)))


  (: has-binding? [-> Env Symbol Boolean])
  (define has-binding?
    (λ (env search-var)
      ((env-has-binding? env) search-var))))
