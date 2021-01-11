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
                (λ ([search-var : Symbol]) : DenVal
                    (if (eqv? search-var var)
                        val
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
                       (λ ([search-var : Symbol]) : DenVal
                           (: look-for-val [-> (Listof Symbol) (Listof DenVal) DenVal])
                           (define look-for-val
                             (λ (vars vals)
                               (cond [(null? vars) (apply-env env search-var)]
                                     [(eqv? search-var (car vars)) (car vals)]
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

  (: extend-env? [-> Env Boolean])
  (define extend-env? (λ (env) (eqv? (env-type env) 'extend-env)))


  (: extend-env-rec [-> Symbol Exp Env Env])
  (define extend-env-rec
    (λ (var exp env)
      (: val (Parameter DenVal))
      (define val (make-parameter (symbol-val 'undefined)))

      (: env Env)
      (define env
        (make-env 'extend-env-rec
                  (λ ([search-var : Symbol]) : Boolean
                      (if (eqv? var search-var)
                          #t
                          (has-binding? env search-var)))
                  (λ ([search-var : Symbol]) : DenVal
                      (if (eqv? search-var var)
                          (val)
                          (apply-env env search-var)))))


      (val (expval->denval (value-of exp env)))

      env))

  (: extend-env-rec+ [-> (Listof (Pair Symbol Exp)) Env Env])
  (define extend-env-rec+
    (λ (exp-bounds saved-env)

      (: val-bounds (Listof (Pair Symbol (Parameter DenVal))))
      (define val-bounds
        (map (ann (λ (exp-bound)
                    (cons (car exp-bound)
                          (make-parameter (symbol-val 'undefined))))
                  [-> (Pair Symbol Exp) (Pair Symbol (Parameter DenVal))])
             exp-bounds))

      (: env Env)
      (define env
        (make-env 'extend-env-rec
                  (λ ([search-var : Symbol]) : Boolean
                      (or (pair? (assoc search-var exp-bounds))
                          (has-binding? saved-env search-var)))
                  (λ ([search-var : Symbol]) : DenVal
                      (: val-bound (Option (Pair Symbol (Parameter DenVal))))
                      (define val-bound (assoc search-var val-bounds))

                      (if (false? val-bound)
                          (apply-env saved-env search-var)
                          ;; lazy evaluate.
                          ((cdr val-bound))))))


      (for-each (ann (λ (val-bound exp-bound)
                       ;; ((cdr val-bound) (expval->denval (value-of (cdr exp-bound) (extend-env+ val-bounds saved-env))))
                       ((cdr val-bound) (expval->denval (value-of (cdr exp-bound) env))))
                     [-> (Pair Symbol (Parameter DenVal))
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

  (: extend-env-rec? [-> Env Boolean])
  (define extend-env-rec? (λ (env) (eqv? (env-type env) 'extend-env-rec)))


  (: env? [-> Any Boolean : Env])
  (define-predicate env? Env)


  (: apply-env [-> Env Symbol DenVal])
  (define apply-env
    (λ (env search-var)
      ((env-apply-env env) search-var)))


  (: has-binding? [-> Env Symbol Boolean])
  (define has-binding?
    (λ (env search-var)
      ((env-has-binding? env) search-var)))

  )
