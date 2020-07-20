#lang typed/racket

(require "../../types/version-1.rkt"
         "../../ExpValues/typed-version-1/values-sig.rkt"
         "../../ExpValues/typed-version-1/values-unit.rkt"
         "../../Environment/typed-version-1/env-sig.rkt"
         "../../Environment/typed-version-1/env-unit.rkt"
         "exp-sig.rkt"
         "exp-unit.rkt")

(define-values/invoke-unit values@
  (import)
  (export values^))

(define-values/invoke-unit env@
  (import)
  (export env^))

(define-values/invoke-unit exp@
  (import values^ env^)
  (export exp^))


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

(displayln (value-of (const-exp -9) (init-env)))
(displayln (value-of (var-exp 'i) (init-env)))
(displayln (value-of (var-exp 'x) (init-env)))
;; (displayln (value-of (var-exp 'y) (init-env)))

(displayln (value-of (unary-exp 'minus (const-exp -9)) (init-env)))
(displayln (value-of (unary-exp 'minus (var-exp 'i))   (init-env)))
(displayln (value-of (unary-exp 'minus (var-exp 'x))   (init-env)))

(displayln (value-of (binary-exp 'greater? (var-exp 'i) (var-exp 'x)) (init-env)))
(displayln (value-of (binary-exp 'less?    (var-exp 'i) (var-exp 'x)) (init-env)))
(displayln (value-of (binary-exp 'equal?   (var-exp 'i) (var-exp 'x)) (init-env)))
(displayln (value-of (binary-exp 'equal?   (var-exp 'i) (var-exp 'i)) (init-env)))


(displayln (value-of (binary-exp 'cons (var-exp 'i) (var-exp 'i)) (init-env)))
(displayln (value-of (nullary-exp 'empty-list) (init-env)))
(displayln (value-of (unary-exp 'car (binary-exp 'cons (var-exp 'i) (var-exp 'x))) (init-env)))
(displayln (value-of (unary-exp 'cdr (binary-exp 'cons (var-exp 'i) (var-exp 'x))) (init-env)))
(displayln (value-of (unary-exp 'null? (binary-exp 'cons (var-exp 'i) (var-exp 'x))) (init-env)))
(displayln (value-of (unary-exp 'null? (nullary-exp 'empty-list)) (init-env)))
(displayln (value-of (n-ary-exp 'list (var-exp 'x) (var-exp 'i) (var-exp 'i)) (init-env)))
