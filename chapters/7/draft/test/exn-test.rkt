#lang typed/racket

(require "../base/base.rkt")

(displayln "Start exn test.\n")

(*eval* '(let ([n 3])
           (with-handlers ([(λ (arg) (eq? arg 2))
                            (λ (arg) (displayln "raise a value: TWO."))]
                           [(λ (arg) (eq? arg 3))
                            (λ (arg) (displayln "raise a value: THREE."))])
             (displayln "1st handlers start")
             (with-handlers ([(λ (arg) (eq? arg 0))
                              (λ (arg) (displayln "raise a value: ZERO."))]
                             [(λ (arg) (eq? arg 1))
                              (λ (arg) (displayln "raise a value: ONE."))])
               (displayln "2nd handlers start")
               (raise n)
               (displayln "2nd handlers end"))
             (displayln "1st handlers end")))
        (base-env) (end-cont))
