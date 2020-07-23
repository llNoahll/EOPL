#lang typed/racket

(require "../types/version-1.rkt"
         "../ExpValues/typed-version-1/values-sig.rkt"
         "../ExpValues/typed-version-1/values-unit.rkt"
         "../Environment/typed-version-1/env-sig.rkt"
         "../Environment/typed-version-1/env-unit.rkt"
         "repl.rkt")

(define-values/invoke-unit values@
  (import)
  (export values^))

(define-values/invoke-unit env@
  (import)
  (export env^))


(: init-env [-> Env])
(define init-env
  (λ ()
    ;; (extend-env 'i (num-val 1)
    ;;             (extend-env 'v (num-val 5)
    ;;                         (extend-env 'x (num-val 10)
    ;;                                     (empty-env))))
    (extend-env* '(i v x)
                 (list (num-val 1) (num-val 5) (num-val 10))
                 (empty-env))))


(displayln (*eval* '2 (init-env)))
(displayln (*eval* '-9 (init-env)))
(displayln (*eval* 'x (init-env)))
(displayln (*eval* 'i (init-env)))

(displayln (*eval* '(minus -9) (init-env)))
(displayln (*eval* '(minus i) (init-env)))
(displayln (*eval* '(minus x) (init-env)))

(displayln (*eval* '(greater? i x) (init-env)))
(displayln (*eval* '(less? i x) (init-env)))

(displayln (*eval* '(cons i i) (init-env)))
(displayln (*eval* '(car (cons i x)) (init-env)))
(displayln (*eval* '(list x i i) (init-env)))
(displayln (*eval* '(null? (empty-list)) (init-env)))
(displayln (*eval* '(cond [(null? (list 1 2 3)) 'cond-1]
                          [(null? (list 9 0 8)) 'cond-2]
                          [else 'else-cons])
                   (empty-env)))
(displayln (*eval* '(cond [(null? (list 1 2 3)) 'cond-1]
                          [(null? (empty-list)) 'cond-2]
                          [else 'else-cons])
                   (empty-env)))


;; (*repl* (empty-env))
