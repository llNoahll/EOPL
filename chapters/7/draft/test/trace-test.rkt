#lang racket

(require "../base/base.rkt")


(displayln (*eval* '((trace-lambda (x) x) 0)
                   (base-env)
                   (end-cont)))
(displayln (*eval* '(let ([fact
                           (Y (trace-lambda (fact)
                                (trace-lambda (n)
                                  (cond [(= n 0) 1]
                                        [(= n 1) 1]
                                        [else (* n (fact (- n 1)))]))))])
                      (fact 5))
                   (base-env)
                   (end-cont)))
(displayln (*eval* '(letrec ([even? (lambda (num)
                                      (cond [(zero? num) #t]
                                            [(= 1 num) #f]
                                            [else (odd? (sub1 num))]))]
                             [odd?  (lambda (num)
                                      (cond [(zero? num) #f]
                                            [(= 1 num) #t]
                                            [else (even? (sub1 num))]))])
                      (displayln "-----------------------")
                      (displayln (even? 0))
                      (displayln (even? 2))
                      (displayln (even? 4))

                      (displayln (odd? 1))
                      (displayln (odd? 3))
                      (displayln (odd? 5)))
                   (base-env)
                   (end-cont)))
