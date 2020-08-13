#lang racket

(require "../types/types.rkt"
         "values-sig.rkt")

(provide values@)


(define-unit values@
  (import)
  (export values^)

  (define symbol-val (λ (symbol) symbol))

  (define num-val (λ (num) num))

  (define bool-val (λ (bool) bool))

  (define char-val (λ (ch) ch))

  (define string-val (λ (str) str))

  (define pair-val (λ (pair) pair))

  (define list-val (λ (ls) ls))

  (define proc-val (λ (proc) proc))


  (define expval->num (λ (val) val))

  (define expval->bool (λ (val) val))

  (define expval->symbol (λ (val) val))

  (define expval->char (λ (val) val))

  (define expval->string (λ (val) val))

  (define expval->pair (λ (val) val))

  (define expval->list (λ (val) val))

  (define expval->proc (λ (val) val))


  (define expval->denval (λ (val) val))


  (define expval->s-expval
    (λ (val)
      (cond [(symbol? val) val]
            [(real? val) val]
            [(boolean? val) val]
            [(char? val) val]
            [(string? val) val]
            [(null? val) val]
            [(list? val)
             (map (λ (arg)
                    (expval->s-expval arg))
                  val)]
            [(pair? val)
             (cons (expval->s-expval (car val))
                   (expval->s-expval (cdr val)))]
            [(proc? val) val]
            [else
             (raise-argument-error 'expval->s-expval "s-expval?" val)])))

  )
