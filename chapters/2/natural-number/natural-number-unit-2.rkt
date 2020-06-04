#lang typed/racket


(require/typed "natural-number-sig.rkt"
  [#:signature natural-number^
   ([zero : [-> My-Zero]]
    [is-zero? : [-> Any Boolean : My-Zero]]
    [successor : [-> My-Natural My-Positive-Int]]
    [predecessor : [-> My-Positive-Int My-Natural]])])


(provide natural-number@
         My-Zero My-Natural My-Positive-Int)

(define-type My-Zero Zero)
(define-type My-Natural Natural)
(define-type My-Positive-Int Positive-Integer)


(define-unit natural-number@
  (import)
  (export natural-number^)

  (: zero My-Zero)
  (define (zero) 0)

  (: is-zero? [-> Any Boolean : My-Zero])
  (define-predicate is-zero? My-Zero)

  (: successor [-> My-Natural My-Positive-Int])
  (define successor (λ (n) (add1 n)))

  (: predecessor [-> My-Positive-Int My-Natural])
  (define predecessor (λ (n) (sub1 n))))


;; (define-values/invoke-unit/infer natural-number@)


(define-values/invoke-unit natural-number@
  (import)
  (export natural-number^))


;; (displayln (successor (zero)))