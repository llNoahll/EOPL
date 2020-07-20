#lang typed/racket

(require "../../types/version-1.rkt"
         "values-sig.rkt")

(provide values@)


(define-unit values@
  (import)
  (export values^)

  (: num-val [-> Integer ExpVal])
  (define num-val (λ (num) num))

  (: bool-val [-> Boolean ExpVal])
  (define bool-val (λ (bool) bool))

  (: pair-val [-> (Pair DenVal DenVal) ExpVal])
  (define pair-val (λ (pair) pair))

  (: list-val [-> (Listof DenVal) ExpVal])
  (define list-val (λ (ls) ls))


  (: expval->num [-> ExpVal Integer])
  (define expval->num
    (λ (val)
      (if (integer? val)
          val
          (raise-argument-error 'expval-num "integer?" val))))

  (: expval->bool [-> ExpVal Boolean])
  (define expval->bool
    (λ (val)
      (if (boolean? val)
          val
          (raise-argument-error 'expval-bool "boolean?" val))))

  (: expval->pair [-> ExpVal (Pair DenVal DenVal)])
  (define expval->pair
    (λ (val)
      (if (pair? val)
          val
          (raise-argument-error 'expval-pair "pair?" val))))

  (: expval->list [-> ExpVal (Listof DenVal)])
  (define expval->list
    (λ (val)
      (if (list? val)
          val
          (raise-argument-error 'expval-list "list?" val))))

  )
