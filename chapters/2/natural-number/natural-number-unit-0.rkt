#lang typed/racket


(require/typed "natural-number-sig.rkt"
  [#:signature natural-number^
   ([zero : [-> My-Natural]]
    ;; [is-zero? : [-> Any Boolean : My-Zero]]
    [is-zero? : [-> My-Natural
                    (All (Nal)
                         [-> [-> Nal Nal] [-> Nal Nal] Nal Boolean])]]
    [successor : [-> My-Natural My-Natural]]
    [predecessor : [-> My-Natural My-Natural]])])

(provide natural-number@ My-Natural)


(define-type (My-Natural Nal) [-> [-> Nal Nal] [-> Nal Nal] Nal Nal])


(define-unit natural-number@
  (import)
  (export natural-number^)

  (: zero [-> My-Natural])
  (define (zero) (λ (1+ 1- o) o))

  (: is-zero? [-> My-Natural
                  (All (Nal)
                       [-> [-> Nal Nal] [-> Nal Nal] Nal Boolean])])
  (define is-zero?
    (λ (num)
      (λ (1+ 1- o)
        (equal? (num 1+ 1- o) o))))

  (: successor [-> My-Natural My-Natural])
  (define successor
    (λ (num)
      (λ (1+ 1- o)
        (1+ (num 1+ 1- o)))))

  (: predecessor [-> My-Natural My-Natural])
  (define predecessor
    (λ (num)
      (λ (1+ 1- o)
        (1- (num 1+ 1- o))))))


;; ;; (define-values/invoke-unit/infer natural-number@)

;; (define-values/invoke-unit natural-number@
;;   (import)
;;   (export natural-number^))


;; ;; test
;; (: -two My-Natural) (define -two (λ (1+ 1- o) (1- (1- o))))
;; (: -one My-Natural) (define -one (λ (1+ 1- o) (1- o)))
;; (: +one My-Natural) (define +one (λ (1+ 1- o) (1+ o)))
;; (: +two My-Natural) (define +two (λ (1+ 1- o) (1+ (1+ o))))


;; (displayln (-two add1 sub1 0))
;; (displayln (-one add1 sub1 0))
;; (displayln ((zero) add1 sub1 0))
;; (displayln (+one add1 sub1 0))
;; (displayln (+two add1 sub1 0))

;; (displayln ((successor (successor (zero))) add1 sub1 0))
;; (displayln ((successor (predecessor (successor (zero)))) add1 sub1 0))
;; (displayln ((predecessor (successor (zero))) add1 sub1 0))
;; (displayln ((predecessor (zero)) add1 sub1 0))

;; (displayln ((is-zero? (zero)) add1 sub1 0))
;; (displayln ((is-zero? (predecessor (successor (zero)))) add1 sub1 0))
;; (displayln ((is-zero? (successor (predecessor (successor (zero))))) add1 sub1 0))
