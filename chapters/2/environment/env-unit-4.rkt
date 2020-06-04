#lang typed/racket

(require/typed "env-sig.rkt"
  [#:signature env^
   ([empty-env    : [-> Empty-Env]]
    [empty-env?   : [-> Any Boolean]]
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

(define-type Empty-Env  (List 'empty-env  [-> Symbol Boolean] [-> Symbol Nothing]))
(define-type Extend-Env (List 'extend-env [-> Symbol Boolean] [-> Symbol Any]))
(define-type Env (U Empty-Env Extend-Env))


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


  (: empty-env [-> Empty-Env])
  (define empty-env
    (let ([empty-environment
           (list 'empty-env
                 (λ ([search-var : Symbol]) : Boolean #f)
                 (λ ([search-var : Symbol]) : Nothing (report-no-binding-found search-var)))])
      (λ ()
        empty-environment)))

  (: empty-env? [-> Any Boolean])
  (define empty-env?
    (λ (arg)
      (eq? arg (empty-env))))


  (: extend-env [-> Symbol Any Env Extend-Env])
  (define extend-env
    (λ (var val env)
      (list 'extend-env
            (λ ([search-var : Symbol]) : Boolean
              (if (eqv? var search-var)
                  #t
                  (has-binding? env search-var)))
            (λ ([search-var : Symbol]) : Any
              (if (eqv? search-var var)
                  val
                  (apply-env env search-var))))))

  (: extend-env* [-> (Pair Symbol (Listof Symbol))
                     (Pair Any (Listof Any))
                     Env
                     Extend-Env])
  (define extend-env*
    (λ (vars vals env)
      (cond [(= (length vars) (length vals))
             (list 'extend-env
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

  (: extend-env? [-> Any Boolean])
  (define extend-env?
    (λ (arg)
      (match arg
        [(list 'extend-env
               (? (λ (proc) (and (procedure? proc) (procedure-arity-includes? proc 1))))
               (? (λ (proc) (and (procedure? proc) (procedure-arity-includes? proc 1)))))
         #t]
        [_ #f])))


  (: env? [-> Any Boolean])
  (define env?
    (λ (arg)
      (or (empty-env? arg) (extend-env? arg))))


  (: apply-env [-> Env Symbol Any])
  (define apply-env
    (λ (env search-var)
      ((caddr env) search-var)))


  (: has-binding? [-> Env Symbol Boolean])
  (define has-binding?
    (λ (env var)
      ((cadr env) var))))
