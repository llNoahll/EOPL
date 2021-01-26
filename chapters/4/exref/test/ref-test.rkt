#lang typed/racket

(require "../base/base.rkt")


(*eval*
 '(let ([x (newref 0)])
    (letrec ([even (λ ()
                     (if (zero? (deref x))
                         1
                         (begin
                           (setref! x (- (deref x) 1))
                           (odd))))]
             [odd  (λ ()
                     (if (zero? (deref x))
                         0
                         (begin
                           (setref! x (- (deref x) 1))
                           (even))))])
      (setref! x 13)
      (odd)))
 (base-env))

(*eval*
 '(let* ([g (let ([counter (newref 0)])
              (λ ()
                (setref! counter (- (deref counter) -1))
                (deref counter)))]
         [a (g)]
         [b (g)])
    (displayln (format "a = ~a, b = ~a" a b)))
 (base-env))

(*eval*
 '(let ([x (newref (newref 0))])
    (setref! (deref x) 11)
    (deref (deref x)))
 (base-env))
