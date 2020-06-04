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
(define-type Extend-Env (Pair (Pair (Pair Symbol (Listof Symbol))
                                    (Pair Any (Listof Any)))
                              Env))
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
      (cons (ann (cons (list var) (list val))
                 (Pair (Pair Symbol (Listof Symbol))
                       (Pair Any (Listof Any))))
            env)))

  (: extend-env* [-> (Pair Symbol (Listof Symbol))
                     (Pair Any (Listof Any))
                     Env
                     Extend-Env])
  (define extend-env*
    (λ (vars vals env)
      (cond [(= (length vars) (length vals))
             (let ([bindings : (Pair (Pair Symbol (Listof Symbol))
                                     (Pair Any (Listof Any)))
                             (cons vars vals)])
               (cons bindings env))]
            [else (error 'extend-env* "Bad input! vars: ~s, vals: ~s" vars vals)])))

  (: extend-env? [-> Any Boolean])
  (define extend-env?
    (λ (arg)
      (match arg
        ;; [(list (cons (list (? symbol?) (? symbol?) ...)
        ;;              (list _  _ ...))
        ;;        ...)
        ;;  #t]
        [(list* (cons (list (? symbol?) (? symbol?) ...)
                      (list _  _ ...))
                (? env?))
         #t]
        [_ #f])))


  (: env? [-> Any Boolean])
  (define env?
    (λ (arg)
      (or (empty-env? arg) (extend-env? arg))))


  (: apply-env [-> Env Symbol Any])
  (define apply-env
    (λ (env search-var)
      (: look-for-val [-> (Listof Symbol) (Listof Any) Any])
      (define look-for-val
        (λ (vars vals)
          (cond [(or (null? vars) (null? vals)) #f]
                [(eqv? search-var (car vars)) (car vals)]
                [else (look-for-val (cdr vars) (cdr vals))])))


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
             (let ([saved-vars : (Pair Symbol (Listof Symbol))
                               (caar env)]
                   [saved-vals : (Pair Any (Listof Any))
                               (cdar env)]
                   [saved-env  : Env (cdr env)])
               (let ([res (look-for-val saved-vars saved-vals)])
                 (if (false? res)
                     (apply-env saved-env search-var)
                     res)))]
            [else (report-invalid-env env)])))


  (: has-binding? [-> Env Symbol Boolean])
  (define has-binding?
    (λ (env var)
      (: exist-var? [-> (Listof Symbol) (Listof Any) Boolean])
      (define exist-var?
        (λ (vars vals)
          (cond [(or (null? vars) (null? vals)) #f]
                [(eqv? var (car vars)) #t]
                [else (exist-var? (cdr vars) (cdr vals))])))

      (cond [(empty-env? env) #f]
            [(exist-var? (caar env) (cdar env)) #t]
            [else (has-binding? (cdr env) var)]))))
