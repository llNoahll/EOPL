#lang typed/racket

(require "../types/types.rkt"
         "../Reference/ref-sig.rkt"
         "../ExpValues/values-sig.rkt"
         "../Procedure/proc-sig.rkt"
         "../Expressions/exp-sig.rkt"
         "env-sig.rkt")

(provide env@)


(: report-no-binding-found [-> Symbol Nothing])
(define report-no-binding-found
  (λ (search-var)
    (error 'apply-env "No binding for ~s" search-var)))

(: report-invalid-env [-> Env Nothing])
(define report-invalid-env
  (λ (env)
    (error 'apply-env "Bad environment: ~s" env)))


(define-unit env@
  (import ref^ values^ proc^ exp^)
  (export env^)


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


  (: extend-env [-> Symbol DenVal Env Env])
  (define extend-env
    (λ (var val saved-env)
      (define ref (newref val))

      (make-env 'extend-env
                (λ ([search-var : Symbol]) : Boolean
                  (if (eqv? var search-var)
                       #t
                       (has-binding? saved-env search-var)))
                (λ ([search-var : Symbol]) : Ref
                  (if (eqv? search-var var)
                      ref
                      (apply-env-ref saved-env search-var))))))

  (: extend-env* [-> (Listof Symbol) (Listof DenVal) Env Env])
  (define extend-env*
    (λ (vars vals saved-env)
      (define refs (map newref vals))

      (cond [(and (null? vars) (null? vals)) saved-env]
            [(= (length vars) (length vals))
             (make-env 'extend-env
                       (λ ([search-var : Symbol]) : Boolean
                         (: exist-var? [-> (Listof Symbol) Boolean])
                         (define exist-var?
                           (λ (vars)
                             (cond [(or (null? vars)) #f]
                                   [(eqv? search-var (car vars)) #t]
                                   [else (exist-var? (cdr vars))])))

                         (if (exist-var? vars)
                             #t
                             (has-binding? saved-env search-var)))
                       (λ ([search-var : Symbol]) : Ref
                         (: look-for-ref [-> (Listof Symbol) (Listof Ref)
                                             ref])
                         (define look-for-ref
                           (λ (vars refs)
                             (cond [(null? vars) (apply-env-ref saved-env search-var)]
                                   [(eqv? search-var (car vars)) (car refs)]
                                   [else (look-for-ref (cdr vars) (cdr refs))])))

                         (look-for-ref vars refs)))]
            [else (error 'extend-env* "Bad input! vars: ~s, vals: ~s" vars vals)])))

  (: extend-env+ [-> (Listof (Pair Symbol DenVal)) Env Env])
  (define extend-env+
    (λ (binds saved-env)
      (extend-env* (map (λ ([bind : (Pair Symbol DenVal)]) : Symbol
                          (car bind))
                        binds)
                   (map (λ ([bind : (Pair Symbol DenVal)]) : DenVal
                          (cdr bind))
                        binds)
                   saved-env)))

  (: extend-env-bind [-> Symbol Ref Env Env])
  (define extend-env-bind
    (λ (var ref saved-env)
      (make-env 'extend-env
                (λ ([search-var : Symbol]) : Boolean
                  (if (eqv? var search-var)
                      #t
                      (has-binding? saved-env search-var)))
                (λ ([search-var : Symbol]) : Ref
                  (if (eqv? search-var var)
                      ref
                      (apply-env-ref saved-env search-var))))))

  (: extend-env-bind* [-> (Listof Symbol) (Listof Ref) Env Env])
  (define extend-env-bind*
    (λ (vars refs saved-env)
      (cond [(and (null? vars) (null? refs)) saved-env]
            [(= (length vars) (length refs))
             (make-env 'extend-env
                       (λ ([search-var : Symbol]) : Boolean
                         (: exist-var? [-> (Listof Symbol) Boolean])
                         (define exist-var?
                           (λ (vars)
                             (cond [(or (null? vars)) #f]
                                   [(eqv? search-var (car vars)) #t]
                                   [else (exist-var? (cdr vars))])))

                         (if (exist-var? vars)
                             #t
                             (has-binding? saved-env search-var)))
                       (λ ([search-var : Symbol]) : Ref
                         (: look-for-ref [-> (Listof Symbol) (Listof Ref)
                                             Ref])
                         (define look-for-ref
                           (λ (vars refs)
                             (cond [(null? vars) (apply-env-ref saved-env search-var)]
                                   [(eqv? search-var (car vars)) (car refs)]
                                   [else (look-for-ref (cdr vars) (cdr refs))])))

                         (look-for-ref vars refs)))]
            [else (error 'extend-env* "Bad input! vars: ~s, refs: ~s" vars refs)])))

  (: extend-env-bind+ [-> (Listof (Pair Symbol Ref)) Env Env])
  (define extend-env-bind+
    (λ (binds saved-env)
      (extend-env-bind* (map (λ ([bind : (Pair Symbol Ref)]) : Symbol
                               (car bind))
                             binds)
                        (map (λ ([bind : (Pair Symbol Ref)]) : Ref
                               (cdr bind))
                             binds)
                        saved-env)))


  (: extend-env? [-> Env Boolean])
  (define extend-env? (λ (env) (eqv? (env-type env) 'extend-env)))


  (: env? [-> Any Boolean : Env])
  (define-predicate env? Env)


  (: apply-env-ref [-> Env Symbol Ref])
  (define apply-env-ref
    (λ (env search-var)
      ((env-apply-env-ref env) search-var)))

  (: apply-env [-> Env Symbol DenVal])
  (define apply-env
    (λ (env search-var)
      (deref ((env-apply-env-ref env) search-var))))


  (: has-binding? [-> Env Symbol Boolean])
  (define has-binding?
    (λ (env search-var)
      ((env-has-binding? env) search-var)))

  (: set-binding! [-> Env Symbol DenVal Void])
  (define set-binding!
    (λ (env search-var new-val)
      (setref! ((env-apply-env-ref env) search-var)
               new-val)))

  )
