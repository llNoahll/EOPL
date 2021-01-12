#lang typed/racket

(require "../types/types.rkt"
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
  (import values^ proc^ exp^)
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
    (λ (var val env)
      (make-env 'extend-env
                (λ ([search-var : Symbol]) : Boolean
                  (if (eqv? var search-var)
                       #t
                       (has-binding? env search-var)))
                (λ ([search-var : Symbol]) : Location
                  (if (eqv? search-var var)
                      (make-parameter val)
                      (apply-env env search-var))))))

  (: extend-env* [-> (Listof Symbol) (Listof DenVal) Env Env])
  (define extend-env*
    (λ (vars vals env)
      (cond [(and (null? vars) (null? vals)) env]
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
                             (has-binding? env search-var)))
                       (λ ([search-var : Symbol]) : Location
                         (: look-for-val [-> (Listof Symbol) (Listof DenVal)
                                             Location])
                         (define look-for-val
                           (λ (vars vals)
                             (cond [(null? vars) (apply-env env search-var)]
                                   [(eqv? search-var (car vars)) (make-parameter (car vals))]
                                   [else (look-for-val (cdr vars) (cdr vals))])))

                         (look-for-val vars vals)))]
            [else (error 'extend-env* "Bad input! vars: ~s, vals: ~s" vars vals)])))

  (: extend-env+ [-> (Listof (Pair Symbol DenVal)) Env Env])
  (define extend-env+
    (λ (binds env)
      (extend-env* (map (λ ([bind : (Pair Symbol DenVal)]) : Symbol
                          (car bind))
                        binds)
                   (map (λ ([bind : (Pair Symbol DenVal)]) : DenVal
                          (cdr bind))
                        binds)
                   env)))

  (: extend-env-bind [-> Symbol Location Env Env])
  (define extend-env-bind
    (λ (var loc env)
      (make-env 'extend-env
                (λ ([search-var : Symbol]) : Boolean
                  (if (eqv? var search-var)
                      #t
                      (has-binding? env search-var)))
                (λ ([search-var : Symbol]) : Location
                  (if (eqv? search-var var)
                      loc
                      (apply-env env search-var))))))

  (: extend-env-bind* [-> (Listof Symbol) (Listof Location) Env Env])
  (define extend-env-bind*
    (λ (vars locs env)
      (cond [(and (null? vars) (null? locs)) env]
            [(= (length vars) (length locs))
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
                             (has-binding? env search-var)))
                       (λ ([search-var : Symbol]) : Location
                         (: look-for-loc [-> (Listof Symbol) (Listof Location)
                                             Location])
                         (define look-for-loc
                           (λ (vars locs)
                             (cond [(null? vars) (apply-env env search-var)]
                                   [(eqv? search-var (car vars)) (car locs)]
                                   [else (look-for-loc (cdr vars) (cdr locs))])))

                         (look-for-loc vars locs)))]
            [else (error 'extend-env* "Bad input! vars: ~s, locs: ~s" vars locs)])))

  (: extend-env-bind+ [-> (Listof (Pair Symbol Location)) Env Env])
  (define extend-env-bind+
    (λ (binds env)
      (extend-env-bind* (map (λ ([bind : (Pair Symbol Location)]) : Symbol
                               (car bind))
                             binds)
                        (map (λ ([bind : (Pair Symbol Location)]) : Location
                               (cdr bind))
                             binds)
                        env)))


  (: extend-env? [-> Env Boolean])
  (define extend-env? (λ (env) (eqv? (env-type env) 'extend-env)))


  (: extend-env-rec [-> Symbol Exp Env Env])
  (define extend-env-rec
    (λ (var exp env)
      (: val Location)
      (define val (make-parameter undefined))

      (: env Env)
      (define env
        (make-env 'extend-env-rec
                  (λ ([search-var : Symbol]) : Boolean
                    (if (eqv? var search-var)
                        #t
                        (has-binding? env search-var)))
                  (λ ([search-var : Symbol]) : Location
                    (if (eqv? search-var var)
                        val
                        (apply-env env search-var)))))


      (val (expval->denval (value-of exp env)))

      env))

  (: extend-env-rec+ [-> (Listof (Pair Symbol Exp)) Env Env])
  (define extend-env-rec+
    (λ (exp-binds saved-env)

      (: val-binds (Listof (Pair Symbol Location)))
      (define val-binds
        (map (ann (λ (exp-bind)
                    (cons (car exp-bind)
                          (ann (make-parameter undefined)
                               Location)))
                  [-> (Pair Symbol Exp)
                      (Pair Symbol Location)])
             exp-binds))

      (: env Env)
      (define env
        (make-env 'extend-env-rec
                  (λ ([search-var : Symbol]) : Boolean
                    (or (pair? (assoc search-var exp-binds))
                        (has-binding? saved-env search-var)))
                  (λ ([search-var : Symbol]) : Location
                    (: val-bind (Option (Pair Symbol Location)))
                    (define val-bind (assoc search-var val-binds))

                    (if (false? val-bind)
                        (apply-env saved-env search-var)
                        ;; lazy evaluate.
                        (cdr val-bind)))))


      (for-each (ann (λ (val-bind exp-bind)
                       ;; ((cdr val-bind) (expval->denval (value-of (cdr exp-bind) (extend-env+ val-binds saved-env))))
                       ((cdr val-bind) (expval->denval (value-of (cdr exp-bind) env))))
                     [-> (Pair Symbol Location)
                         (Pair Symbol Exp)
                         Void])
                val-binds exp-binds)

      env))

  (: extend-env-rec* [-> (Listof Symbol) (Listof Exp) Env Env])
  (define extend-env-rec*
    (λ (vars exps saved-env)
      (extend-env-rec+ (map (ann (λ (var exp)
                                   (cons var exp))
                                 [-> Symbol Exp (Pair Symbol Exp)])
                            vars exps)
                       saved-env)))

  (: extend-env-rec-bind [-> Symbol Location Env Env])
  (define extend-env-rec-bind
    (λ (var loc env)
      (make-env 'extend-env-rec
                (λ ([search-var : Symbol]) : Boolean
                  (if (eqv? var search-var)
                      #t
                      (has-binding? env search-var)))
                (λ ([search-var : Symbol]) : Location
                  (if (eqv? search-var var)
                      loc
                      (apply-env env search-var))))))

  (: extend-env-rec-bind+ [-> (Listof (Pair Symbol Location)) Env Env])
  (define extend-env-rec-bind+
    (λ (loc-binds saved-env)
      (make-env 'extend-env-rec
                (λ ([search-var : Symbol]) : Boolean
                  (or (pair? (assoc search-var loc-binds))
                      (has-binding? saved-env search-var)))
                (λ ([search-var : Symbol]) : Location
                  (: loc-bind (Option (Pair Symbol Location)))
                  (define loc-bind (assoc search-var loc-binds))

                  (if (false? loc-bind)
                      (apply-env saved-env search-var)
                      ;; lazy evaluate.
                      (cdr loc-bind))))))

  (: extend-env-rec-bind* [-> (Listof Symbol) (Listof Location) Env Env])
  (define extend-env-rec-bind*
    (λ (vars locs saved-env)
      (extend-env-rec-bind+ (map (ann (λ (var loc)
                                        (cons var loc))
                                      [-> Symbol Location (Pair Symbol Location)])
                                 vars locs)
                            saved-env)))


  (: extend-env-rec? [-> Env Boolean])
  (define extend-env-rec? (λ (env) (eqv? (env-type env) 'extend-env-rec)))


  (: env? [-> Any Boolean : Env])
  (define-predicate env? Env)


  (: apply-env [-> Env Symbol Location])
  (define apply-env
    (λ (env search-var)
      ((env-apply-env env) search-var)))


  (: has-binding? [-> Env Symbol Boolean])
  (define has-binding?
    (λ (env search-var)
      ((env-has-binding? env) search-var)))

  )
