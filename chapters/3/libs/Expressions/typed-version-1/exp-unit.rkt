#lang typed/racket

(require "../../types/version-1.rkt"
         "../../ExpValues/typed-version-1/values-sig.rkt"
         "../../Environment/typed-version-1/env-sig.rkt"
         "exp-sig.rkt")

(provide exp@)


(define-unit exp@
  (import values^ env^)
  (export exp^)

  (: const-exp [-> Integer Exp])
  (define const-exp (λ (num) (make-const-exp num)))

  (: zero?-exp [-> Exp Exp])
  (define zero?-exp (λ (exp) (make-zero?-exp exp)))

  (: if-exp [-> Exp Exp Exp Exp])
  (define if-exp
    (λ (pred-exp true-exp false-exp)
      (make-if-exp pred-exp true-exp false-exp)))

  (: diff-exp [-> Exp Exp Exp])
  (define diff-exp
    (λ (minuend-exp subtrahend-exp)
      (make-diff-exp minuend-exp subtrahend-exp)))

  (: var-exp [-> Symbol Exp])
  (define var-exp (λ (var) (make-var-exp var)))

  (: let-exp [-> Symbol Exp Exp Exp])
  (define let-exp
    (λ (bound-var bound-exp body)
      (make-let-exp bound-var bound-exp body)))


  (: value-of [-> Exp Env ExpVal])
  (define value-of
    (λ (exp env)
      (cond [(const-exp? exp)
             (num-val (const-exp-num exp))]
            [(var-exp? exp) (cast (apply-env env (var-exp-var exp)) ExpVal)]
            [(diff-exp? exp)
             (let ([minuend-val (value-of (diff-exp-minuend-exp exp) env)]
                   [subtrahend-val (value-of (diff-exp-subtrahend-exp exp) env)])
               (let ([minuend-num (expval->num minuend-val)]
                     [subtrahend-num (expval->num subtrahend-val)])
                 (- minuend-num subtrahend-num)))]
            [(if-exp? exp)
             (let ([pred-val (value-of (if-exp-pred-exp exp) env)])
               (if (expval->bool pred-val)
                   (value-of (if-exp-true-exp exp) env)
                   (value-of (if-exp-false-exp exp) env)))]
            [(zero?-exp? exp)
             (let* ([val (value-of (zero?-exp-exp exp) env)]
                    [num (expval->num val)])
               (if (zero? num)
                   (bool-val #t)
                   (bool-val #f)))]
            [(let-exp? exp)
             (let ([val (value-of (let-exp-bound-exp exp) env)])
               (value-of (let-exp-body exp)
                         (extend-env (let-exp-bound-var exp)
                                     val
                                     env)))]
            [else (raise-argument-error 'value-of "exp?" exp)])))
  )
