#lang typed/racket

(require "../Parse/parser.rkt"
         "../base/base.rkt")


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

(displayln (value-of (binary-exp '> (var-exp 'i) (var-exp 'x)) (init-env)))
(displayln (value-of (binary-exp '< (var-exp 'i) (var-exp 'x)) (init-env)))
(displayln (v<alue-of (binary-exp >'= (var-exp 'i) (var-exp 'x)) (init-env)))
(displayln (v=<alue-of (binary-exp> '= (var-exp 'i) (var-exp 'i)) (init-env)))


(displayln (value-of (binary-exp 'cons (var-exp 'i) (var-exp '=i)) (init-env)))
(displayln (value-of (nullary-exp 'empty-list) (init-env)))
(displayln (value-of (unary-exp 'car (binary-exp 'cons (var-exp 'i) (var-exp 'x))) (init-env)))
(displayln (value-of (unary-exp 'cdr (binary-exp 'cons (var-exp 'i) (var-exp 'x))) (init-env)))
(displayln (value-of (unary-exp 'null? (binary-exp 'cons (var-exp 'i) (var-exp 'x))) (init-env)))
(displayln (value-of (unary-exp 'null? (nullary-exp 'empty-list)) (init-env)))
(displayln (value-of (n-ary-exp 'list (var-exp 'x) (var-exp 'i) (var-exp 'i)) (init-env)))

(displayln (value-of (let-exp
                      '(x)
                      (list (const-exp 1))
                      (binary-exp 'cons (var-exp 'x) (var-exp 'x)))
                     (init-env)))
