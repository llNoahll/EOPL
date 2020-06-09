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
          (raise-argument-error 'expval-bool "boolean?" val)))))
