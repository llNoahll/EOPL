#lang typed/racket

(require "../natural-number/natural-number-unit-1.rkt")

(require/typed "../natural-number/natural-number-sig.rkt"
  [#:signature natural-number^
   ([zero : [-> My-Zero]]
    [is-zero? : [-> Any Boolean : My-Zero]]
    ;; [is-pos-int? : [-> Any Boolean : My-Positive-Int]]
    ;; [is-natural? : [-> Any Boolean : My-Natural]]
    [successor : [-> My-Natural My-Positive-Int]]
    [predecessor : [-> My-Positive-Int My-Natural]])])


;; (define-values/invoke-unit/infer natural-number@)

(define-values/invoke-unit natural-number@
  (import)
  (export natural-number^))


;; (: add (case-> [-> My-Zero]
;;                [-> My-Natural My-Natural]
;;                [-> My-Natural My-Natural My-Natural]))
;; (define add
;;   (case-lambda
;;     [() (zero)]
;;     [(num) num]
;;     [(num-1 num-2)
;;      (cond [(is-zero? num-1) num-2]
;;            [else
;;             (successor (add (predecessor num-1) num-2))])]))

(: add (case-> [-> My-Zero]
               [-> My-Natural * My-Natural]))
(define add
  (λ nums
    (match nums
      [(? null?) (zero)]
      [`(,num) num]
      [`(,num-1 ,(? is-zero? num-2)) num-1]
      [`(,num-1 ,num-2)
       (cond [(is-zero? num-1) num-2]
             [else
              (successor (add (predecessor num-1) num-2))])]
      [(list num-1 num-2 nums ...)
       (apply add (add num-1 num-2) nums)])))

(: mul (case-> [-> My-Natural * My-Natural]))
(define mul
  (λ nums
    (match nums
      [(? null?) (successor (zero))]
      [`(,num) num]
      [`(,num-1 ,num-2)
       (cond [(is-zero? num-1) (zero)]
             [(is-zero? num-2) (zero)]
             [(is-zero? (predecessor num-1)) num-2]
             [(is-zero? (predecessor num-2)) num-1]
             [else
              (add (mul (predecessor num-1) num-2)
                   num-2)])]
      [(list num-1 num-2 nums ...)
       (apply mul (mul num-1 num-2) nums)])))


(: make-natural [-> Natural My-Natural])
(define make-natural
  (λ (num)
    (if (zero? num)
        (zero)
        (successor (make-natural (sub1 num))))))


(: fact [-> My-Natural My-Natural])
(define fact
  (λ (num)
    (if (is-zero? num)
        (make-natural 1)
        (mul num (fact (predecessor num))))))


;; test

(: +one My-Positive-Int) (define +one (successor (zero)))
(: +two My-Positive-Int) (define +two (successor +one))


(displayln (zero))
(displayln +one)
(displayln +two)

(displayln (successor (successor (zero))))
(displayln (successor (predecessor (successor (zero)))))
(displayln (predecessor (successor (zero))))
(displayln (predecessor +one))

(displayln (is-zero? (zero)))
(displayln (is-zero? (predecessor (successor (zero)))))
(displayln (is-zero? (successor (predecessor (successor (zero))))))


(displayln (make-natural 6))
(displayln (make-natural 7))
(displayln (make-natural 8))

(displayln (add +two +two))
(displayln (add +two +two +two +one))

(displayln (mul +two +two))
(displayln (mul (make-natural 5)
                (make-natural 7)
                (make-natural 2)))

(displayln (fact (make-natural 0)))
(displayln (fact (make-natural 1)))
(displayln (fact (make-natural 2)))
(displayln (fact (make-natural 3)))
(displayln (fact (make-natural 4)))
(displayln (fact (make-natural 5)))

;; (displayln (length (make-natural 6)))
;; (displayln (length (make-natural 7)))
;; (displayln (length (make-natural 8)))

;; (displayln (length (add +two +two)))
;; (displayln (length (add +two +two +two +one)))

;; (displayln (length (mul +two +two)))
;; (displayln (length (mul (make-natural 5)
;;                         (make-natural 7)
;;                         (make-natural 2))))

;; (displayln (length (fact (make-natural 0))))
;; (displayln (length (fact (make-natural 1))))
;; (displayln (length (fact (make-natural 2))))
;; (displayln (length (fact (make-natural 3))))
;; (displayln (length (fact (make-natural 4))))
;; (displayln (length (fact (make-natural 5))))