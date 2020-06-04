#lang racket

(require "integer-number-sig.rkt")

(provide integer-number@)


(define-unit integer-number@
  (import)
  (export integer-number^)


  (define (zero) (cons 1 1))

  (define is-zero?
    (λ (num)
      (if (list? num)
          (diff-tree=? (car num) (cdr num))
          #f)))

  (define successor
    (λ (num)
      (cons num (opposite 1))))

  (define predecessor
    (λ (num)
      (cons num 1))))


(define-values/invoke-unit integer-number@
  (import)
  (export integer-number^))


(define make-diff-tree
  (λ (num)
    (define make-non-neg
      (λ (num)
        (if (zero? num)
            (zero)
            (successor (make-non-neg (sub1 num))))))

    (define make-non-pos
      (λ (num)
        (if (zero? num)
            (zero)
            (predecessor (make-non-pos (add1 num))))))

    (cond [(natural? num) (make-non-neg num)]
          [else (make-non-pos num)])))

(define diff-tree->integer
  (λ (num)
    (match num
      [1 1]
      [`(  1 . ,t2) (- 1 (diff-tree->integer t2))]
      [`(,t1 .   1) (- (diff-tree->integer t1) 1)]
      [`(,t1 . ,t2) (- (diff-tree->integer t1) (diff-tree->integer t2))])))

(define diff-tree=?
  (λ (num-1 num-2)
    (= (diff-tree->integer num-1)
       (diff-tree->integer num-2))))

(define opposite
  (λ (num)
    (cons (zero) num)))

(define diff-tree-plus
  (λ (num-1 num-2)
    (cons num-1 (opposite num-2))))


;; (displayln (zero))
;; (displayln (diff-tree->integer (zero)))

;; (displayln (make-diff-tree 0))
;; (displayln (make-diff-tree 1))

;; (displayln (diff-tree->integer (make-diff-tree 0)))
;; (displayln (diff-tree->integer (make-diff-tree 1)))
;; (displayln (diff-tree->integer (make-diff-tree 2)))
;; (displayln (diff-tree->integer (make-diff-tree 3)))
;; (displayln (diff-tree->integer (make-diff-tree 4)))
;; (displayln (diff-tree->integer (make-diff-tree 5)))
;; (displayln (diff-tree->integer (make-diff-tree 6)))
;; (displayln (diff-tree->integer (make-diff-tree 7)))


;; (displayln (diff-tree->integer (diff-tree-plus (make-diff-tree 8)
;;                                                (make-diff-tree 12))))