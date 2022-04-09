#lang typed/racket

(require "../Parse/desugar.rkt")


(pretty-print (desugar '(let* ([x (- x 1)]
                               [y (- x 2)])
                          (+ x y)
                          (* x y)
                          (- x y))))

(pretty-print (desugar '(let ([x 30])
                          (let* ([x (- x 1)]
                                 [y (- x 2)])
                            (+ x y)
                            (* x y)
                            (- x y)))))
