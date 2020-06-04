#lang typed/racket

(require/typed "env-sig.rkt"
  [#:signature env^
   ([empty-env    : [-> Empty-Env]]
    [empty-env?   : [-> Any Boolean : Empty-Env]]
    [extend-env   : [-> Symbol Any Env Extend-Env]]
    [extend-env*  : [-> (Pair Symbol (Listof Symbol))
                        (Pair Any (Listof Any))
                        Env
                        Extend-Env]]
    [extend-env?  : [-> Any Boolean]]
    [env?         : [-> Any Boolean]]
    [apply-env    : [-> Env Symbol Any]]
    [has-binding? : [-> Env Symbol Boolean]])])

(provide env@ Empty-Env Extend-Env Env)

(define-type Empty-Env  '())
(define-type Extend-Env (Pair (Pair Symbol Any) Env))
(define-type Env (U Empty-Env Extend-Env))


(define-unit env@
  (import)
  (export env^)

  (: empty-env [-> Empty-Env])
  (define (empty-env) '())

  (: empty-env? [-> Any Boolean : Empty-Env])
  (define-predicate empty-env? Empty-Env)


  (: extend-env [-> Symbol Any Env Extend-Env])
  (define extend-env
    (λ (var val env)
      (cons (cons var val) env)))

  (: extend-env* [-> (Pair Symbol (Listof Symbol))
                     (Pair Any (Listof Any))
                     Env
                     Extend-Env])
  (define extend-env*
    (λ (vars vals env)
      (: extend-env*-iter [-> (Listof Symbol) (Listof Any) Extend-Env Extend-Env])
      (define extend-env*-iter
        (λ (vars vals env)
          (if (or (null? vars) (null? vals))
              env
              (extend-env*-iter (cdr vars) (cdr vals)
                                (extend-env (car vars) (car vals) env)))))

      (cond [(= (length vars) (length vals))
             (extend-env*-iter (cdr vars) (cdr vals)
                               (extend-env (car vars) (car vals) env))]
            [else (error 'extend-env* "Bad input! vars: ~s, vals: ~s" vars vals)])))

  (: extend-env? [-> Any Boolean])
  (define extend-env?
    (λ (arg)
      (match arg
        [`((,(? symbol?) . ,_) . ,(? env?)) #t]
        [_ #f])))


  (: env? [-> Any Boolean])
  (define env?
    (λ (arg)
      (or (empty-env? arg) (extend-env? arg))))


  (: apply-env [-> Env Symbol Any])
  (define apply-env
    (λ (env search-var)
      (: report-no-binding-found [-> Symbol Nothing])
      (define report-no-binding-found
        (λ (search-var)
          (error 'apply-env "No binding for ~s in ~s" search-var env)))

      (: report-invalid-env [-> Env Nothing])
      (define report-invalid-env
        (λ (env)
          (error 'apply-env "Bad environment: ~s" env)))


      (cond [(empty-env? env)
             (report-no-binding-found search-var)]
            [(extend-env? env)
             (let ([saved-var : Symbol (caar env)]
                   [saved-val : Any (cdar env)]
                   [saved-env : Env (cdr env)])
               (if (eqv? search-var saved-var)
                   saved-val
                   (apply-env saved-env search-var)))]
            [else (report-invalid-env env)])))


  (: has-binding? [-> Env Symbol Boolean])
  (define has-binding?
    (λ (env var)
      (cond [(empty-env? env) #f]
            [(eqv? var (caar env)) #t]
            [else (has-binding? (cdr env) var)]))))
