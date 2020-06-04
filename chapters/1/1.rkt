#! /usr/bin/env racket
#lang typed/racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(: in-s? [-> Natural Boolean])
(define in-s?
  (λ (n)
    (let ([m (- n 3)])
      (cond [(zero? n) #t]
            [(natural? m) (in-s? m)]
            [else #f]))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(: list-length [-> (Listof Any) Natural])
(define list-length
  (λ (lst)
    (cond [(null? lst) 0]
          [else (add1 (list-length (cdr lst)))])))


(: nth-element [-> (Listof Any) Natural Any])
(define nth-element
  (λ (lst n)
    (: nth-element-rec [-> (Listof Any) Natural Any])
    (define nth-element-rec
      (λ (lst n)
        (cond [(null? lst) #f]
              [(zero? n) (car lst)]
              [else (nth-element-rec (cdr lst) (sub1 n))])))

    (let ([ans (nth-element-rec lst n)])
      (if ans ans (report-list-too-short lst n)))))

(: report-list-too-short [-> (Listof Any) Natural Void])
(define report-list-too-short
  (λ (lst n)
    (error 'nth-element
           "~s doesn't have ~s elements.~%" lst (add1 n))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(: remove-first [-> Symbol (Listof Symbol) (Listof Symbol)])
(define remove-first
  (λ (s los)
    (cond [(null? los) los]
          [(eqv? s (car los)) (cdr los)]
          [else (cons (car los) (remove-first s (cdr los)))])))


(: remove-all [-> Symbol (Listof Symbol) (Listof Symbol)])
(define remove-all
  (λ (s los)
    (cond [(null? los) los]
          [(eqv? s (car los)) (remove-all s (cdr los))]
          [else (cons (car los) (remove-all s (cdr los)))])))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type LcExpr (U Symbol Lambda (List LcExpr LcExpr)))
(: lc-expr? [-> Any Boolean : LcExpr])
(define-predicate lc-expr? LcExpr)

(define-type Lambda (List (U 'λ 'lambda) (List Symbol) LcExpr))
(: lambda? [-> Any Boolean : Lambda])
(define-predicate lambda? Lambda)


(: occurs-free? [-> Symbol LcExpr Boolean])
(define occurs-free?
  (λ (val expr)
    (cond [(symbol? expr) (eqv? val expr)]
          [(lambda? expr)
           (and (not (eqv? (caadr expr) val))
                (occurs-free? val (caddr expr)))]
          [else
           (or (occurs-free? val (car expr))
               (occurs-free? val (cadr expr)))])))
;; (define occurs-free?
;;   (λ (val expr)
;;     (match expr
;;       [x #:when (symbol? expr)
;;          (eqv? val expr)]
;;       [(list lam (list id) (? lc-expr? lc-expr))
;;        #:when (or (eqv? lam 'lambda)
;;                   (eqv? lam 'λ))
;;        (and (not (eqv? id val))
;;             (occurs-free? val lc-expr))]
;;       [(list (? lc-expr? lc-expr-1) (? lc-expr? lc-expr-2))
;;        (or (occurs-free? val lc-expr-1)
;;            (occurs-free? val lc-expr-2))])))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type S-List (U Null (Pair S-Expr S-List)))
(define-type S-Expr (U Symbol S-List))

(: subst [-> Symbol Symbol S-List S-List])
(define subst
  (λ (s-new s-old s-list)
    (: subst-in-s-expr [-> Symbol Symbol S-Expr S-Expr])
    (define subst-in-s-expr
      (λ (s-new s-old s-expr)
        (if (symbol? s-expr)
            (if (eqv? s-old s-expr) s-new s-expr)
            (subst s-new s-old s-expr))))

    (if (null? s-list)
        '()
        (cons (subst-in-s-expr s-new s-old (car s-list))
              (subst s-new s-old (cdr s-list))))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(: number-elements [-> (Listof Any) (Listof (List Natural Any))])
(define number-elements
  (λ (lst)
    (: number-elements-from [-> (Listof Any) Natural
                                (Listof (List Natural Any))])
    (define number-elements-from
      (λ (lst num)
        (if (null? lst) '()
            (cons (list num (car lst))
                  (number-elements-from (cdr lst)
                                        (add1 num))))))

    (number-elements-from lst 0)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(: list-sum [-> (Listof Number) Number])
(define list-sum
  (λ (loi)
    (if (null? loi)
        0
        (+ (car loi)
           (list-sum (cdr loi))))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(: vector-sum [-> (Vectorof Number) Number])
(define vector-sum
  (λ (v)
    (: partial-vector-sum [-> (Vectorof Number) Natural Number])
    (define partial-vector-sum
      (λ (v n)
        (if (zero? n)
            (vector-ref v 0)
            (+ (vector-ref v n)
               (partial-vector-sum v (sub1 n))))))

    (let ([n (vector-length v)])
      (if (zero? n)
          0
          (partial-vector-sum v (sub1 n))))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
