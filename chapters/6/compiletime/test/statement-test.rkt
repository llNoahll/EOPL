#lang racket

(require "../base/base.rkt")


(*eval*
 '(let ([x '()]
        [y '()])
    (set! x 3)
    (set! y 4)

    (displayln (+ x y)))
 (base-env)
 (end-cont))
