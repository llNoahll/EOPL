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
    (λ (bounds env)
      (extend-env* (map (λ ([bound : (Pair Symbol DenVal)]) : Symbol
                          (car bound))
                        bounds)
                   (map (λ ([bound : (Pair Symbol DenVal)]) : DenVal
                          (cdr bound))
                        bounds)
                   env)))

  (: extend-env-bound [-> Symbol Location Env Env])
  (define extend-env-bound
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

  (: extend-env-bound* [-> (Listof Symbol) (Listof Location) Env Env])
  (define extend-env-bound*
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

  (: extend-env-bound+ [-> (Listof (Pair Symbol Location)) Env Env])
  (define extend-env-bound+
    (λ (bounds env)
      (extend-env-bound* (map (λ ([bound : (Pair Symbol Location)]) : Symbol
                                (car bound))
                              bounds)
                         (map (λ ([bound : (Pair Symbol Location)]) : Location
                                (cdr bound))
                              bounds)
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
    (λ (exp-bounds saved-env)

      (: val-bounds (Listof (Pair Symbol Location)))
      (define val-bounds
        (map (ann (λ (exp-bound)
                    (cons (car exp-bound)
                          (ann (make-parameter undefined)
                               Location)))
                  [-> (Pair Symbol Exp)
                      (Pair Symbol Location)])
             exp-bounds))

      (: env Env)
      (define env
        (make-env 'extend-env-rec
                  (λ ([search-var : Symbol]) : Boolean
                      (or (pair? (assoc search-var exp-bounds))
                          (has-binding? saved-env search-var)))
                  (λ ([search-var : Symbol]) : Location
                      (: val-bound (Option (Pair Symbol Location)))
                      (define val-bound (assoc search-var val-bounds))

                      (if (false? val-bound)
                          (apply-env saved-env search-var)
                          ;; lazy evaluate.
                          (cdr val-bound)))))


      (for-each (ann (λ (val-bound exp-bound)
                       ;; ((cdr val-bound) (expval->denval (value-of (cdr exp-bound) (extend-env+ val-bounds saved-env))))
                       ((cdr val-bound) (expval->denval (value-of (cdr exp-bound) env))))
                     [-> (Pair Symbol Location)
                         (Pair Symbol Exp)
                         Void])
           val-bounds exp-bounds)

      env))

  (: extend-env-rec* [-> (Listof Symbol) (Listof Exp) Env Env])
  (define extend-env-rec*
    (λ (vars exps saved-env)
      (extend-env-rec+ (map (ann (λ (var exp)
                                   (cons var exp))
                                 [-> Symbol Exp (Pair Symbol Exp)])
                            vars exps)
                       saved-env)))

  (: extend-env-rec-bound [-> Symbol Location Env Env])
  (define extend-env-rec-bound
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

  (: extend-env-rec-bound+ [-> (Listof (Pair Symbol Location)) Env Env])
  (define extend-env-rec-bound+
    (λ (loc-bounds saved-env)
      (make-env 'extend-env-rec
                (λ ([search-var : Symbol]) : Boolean
                    (or (pair? (assoc search-var loc-bounds))
                        (has-binding? saved-env search-var)))
                (λ ([search-var : Symbol]) : Location
                    (: loc-bound (Option (Pair Symbol Location)))
                    (define loc-bound (assoc search-var loc-bounds))

                    (if (false? loc-bound)
                        (apply-env saved-env search-var)
                        ;; lazy evaluate.
                        (cdr loc-bound))))))

  (: extend-env-rec-bound* [-> (Listof Symbol) (Listof Location) Env Env])
  (define extend-env-rec-bound*
    (λ (vars locs saved-env)
      (extend-env-rec-bound+ (map (ann (λ (var loc)
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
