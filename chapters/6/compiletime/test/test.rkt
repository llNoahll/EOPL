#lang typed/racket

(require "../base/base.rkt")


(value-of/k
 (call-exp
  (proc-exp
   '(x)
   (proc-exp
    '()
    (begin-exp
      (list
       (if-exp
        (call-exp (var-exp 'zero?) (list (var-exp 'x)))
        (real-exp 1)
        (begin-exp
          (list
           #;(assign-exp
              'x
              (call-exp (var-exp '-) (list (var-exp 'x) (real-exp 1))))
           (var-exp 'x))))))))
  (list (real-exp 10)))
 (base-env)
 (end-cont))
