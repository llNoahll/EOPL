#lang typed/racket

(require "../base/base.rkt")


(*eval* '(let ([n 0])
           (set! n (+ n 1))
           n)
        (base-env))


(*eval*
 '(let ([x 0])
    (letrec ([even (λ ()
                     (if (zero? x)
                         1
                         (begin
                           (set! x (- x 1))
                           (odd))))]
             [odd  (λ ()
                     (if (zero? x)
                         0
                         (begin
                           (set! x (- x 1))
                           (even))))])
      (set! x 13)
      (odd)))
 (base-env))


(*eval*
 '(let* ([g (let ([counter 0])
              (λ ()
                (set! counter (- counter -1))
                counter))]
         [a (g)]
         [b (g)])
    (displayln (format "a = ~a, b = ~a" a b)))
 (base-env))
