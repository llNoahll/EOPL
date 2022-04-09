#lang typed/racket

(require "../types/types.rkt"
         "tenv-sig.rkt")

(provide tenv@)


(: report-no-type-binding-found [-> Symbol Nothing])
(define report-no-type-binding-found
  (λ (search-var)
    (error 'apply-env "No type binding for ~s" search-var)))

(: report-invalid-tenv [-> TEnv Nothing])
(define report-invalid-tenv
  (λ (tenv)
    (error 'apply-env "Bad type environment: ~s" tenv)))


(define-unit tenv@
  (import)
  (export tenv^)


  (: base-tenv  (Parameter TEnv))
  (: empty-tenv [-> TEnv])
  (define-values (base-tenv empty-tenv)
    (let ([empty-type-environment (make-tenv 'empty-tenv #hasheq())])
      (values (make-parameter empty-type-environment)
              (λ () empty-type-environment))))

  (: empty-tenv? [-> TEnv Boolean])
  (define empty-tenv? (λ (tenv) (eqv? (tenv-type tenv) 'empty-tenv)))


  (: extend-tenv [-> Symbol Type TEnv TEnv])
  (define extend-tenv
    (λ (var val saved-tenv)
      (make-tenv 'extend-tenv (hash-set (tenv-binds saved-tenv) var val))))

  (: extend-tenv* [-> (Listof Symbol) (Listof Type) TEnv TEnv])
  (define extend-tenv*
    (λ (vars vals saved-tenv)
      (unless (= (length vars) (length vals))
        (raise-arguments-error 'extend-tenv*
                               "The number of formal arguments and actual arguments is not equal."
                               "formal arguments" vars
                               "actual arguments" vals))

      (make-tenv 'extend-tenv
                 (for/fold ([res : (Immutable-HashTable Symbol Type)
                                 (tenv-binds saved-tenv)])
                           ([var (in-list vars)]
                            [val (in-list vals)])
                   (hash-set res var val)))))

  (: extend-tenv+ [-> (Listof (Pair Symbol Type)) TEnv TEnv])
  (define extend-tenv+
    (λ (binds saved-tenv)
      (make-tenv 'extend-tenv
                 (for/fold ([res : (Immutable-HashTable Symbol Type)
                                 (tenv-binds saved-tenv)])
                           ([bind (in-list binds)])
                   (hash-set res (car bind) (cdr bind))))))


  (: extend-tenv? [-> TEnv Boolean])
  (define extend-tenv? (λ (tenv) (eqv? (tenv-type tenv) 'extend-tenv)))


  (: tenv? [-> Any Boolean : TEnv])
  (define-predicate tenv? TEnv)


  (: apply-tenv [-> TEnv Symbol Type])
  (define apply-tenv
    (λ (tenv var)
      (hash-ref (tenv-binds tenv) var)))


  (: has-tbinding? [-> TEnv Symbol Boolean])
  (define has-tbinding?
    (λ (tenv var)
      (hash-has-key? (tenv-binds tenv) var)))

  )
