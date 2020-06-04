#lang typed/racket


(require/typed "natural-number-sig.rkt"
  [#:signature natural-number^
   ([zero : [-> My-Zero]]
    [is-zero? : [-> Any Boolean : My-Zero]]
    ;; [is-pos-int? : [-> Any Boolean : My-Positive-Int]]
    ;; [is-natural? : [-> Any Boolean : My-Natural]]
    [successor : [-> My-Natural My-Positive-Int]]
    [predecessor : [-> My-Positive-Int My-Natural]])])

(provide natural-number@
         My-Zero My-Natural My-Positive-Int)


(define-type My-Zero Null)
(define-type My-Positive-Int (Pairof #t (Listof #t)))
(define-type My-Natural (U My-Zero My-Positive-Int))



;; (: natural-number@ [Unit (import)
;;                          (export natural-number^)
;;                          Void])
;; (define natural-number@
;;   (unit
;;    (import)
;;    (export natural-number^)


;;    (: zero [-> My-Zero])
;;    (define (zero) (λ () '()))

;;    (: is-zero? [-> Any Boolean])
;;    (define is-zero? (λ (n) (null? n)))

;;    (: successor [-> My-Natural My-Positive-Int])
;;    (define successor (λ (n) (cons #t n)))

;;    (: predecessor [-> My-Positive-Int My-Natural])
;;    (define predecessor (λ (n) (cdr n)))))



(define-unit natural-number@
  (import)
  (export natural-number^)

  (: zero My-Zero)
  (define (zero) '())

  (: is-zero? [-> Any Boolean : My-Zero])
  (define-predicate is-zero? My-Zero)

  ;; (: is-pos-int? [-> Any Boolean : My-Positive-Int])
  ;; (define-predicate is-pos-int? My-Positive-Int)

  (: is-natural? [-> Any Boolean : My-Natural])
  (define-predicate is-natural? My-Natural)

  (: successor [-> My-Natural My-Positive-Int])
  (define successor (λ (n) (cons #t n)))

  (: predecessor [-> My-Positive-Int My-Natural])
  (define predecessor (λ (n) (cdr n))))


;; (define-values/invoke-unit/infer natural-number@)

;; (define-values/invoke-unit natural-number@
;;   (import)
;;   (export natural-number^))


;; ;; test

;; (: +one My-Positive-Int) (define +one (successor (zero)))
;; (: +two My-Positive-Int) (define +two (successor +one))


;; (displayln (zero))
;; (displayln +one)
;; (displayln +two)

;; (displayln (successor (successor (zero))))
;; (displayln (successor (predecessor (successor (zero)))))
;; (displayln (predecessor (successor (zero))))
;; (displayln (predecessor +one))

;; (displayln (is-zero? (zero)))
;; (displayln (is-zero? (predecessor (successor (zero)))))
;; (displayln (is-zero? (successor (predecessor (successor (zero))))))
