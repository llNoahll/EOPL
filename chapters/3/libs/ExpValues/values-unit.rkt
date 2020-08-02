#lang typed/racket

(require "../types/types.rkt"
         "values-sig.rkt")

(provide values@)


(define-unit values@
  (import)
  (export values^)

  (: symbol-val [-> Symbol DenVal])
  (define symbol-val (λ (symbol) symbol))

  (: num-val [-> Integer DenVal])
  (define num-val (λ (num) num))

  (: bool-val [-> Boolean DenVal])
  (define bool-val (λ (bool) bool))

  (: char-val [-> Char DenVal])
  (define char-val (λ (ch) ch))

  (: string-val [-> String DenVal])
  (define string-val (λ (str) str))

  (: pair-val [-> (Pair DenVal DenVal) DenVal])
  (define pair-val (λ (pair) pair))

  (: list-val [-> (Listof DenVal) DenVal])
  (define list-val (λ (ls) ls))

  (: proc-val [-> Proc DenVal])
  (define proc-val (λ (proc) proc))


  (: expval->num [-> ExpVal Integer])
  (define expval->num (λ (val) (cast val Integer)))

  (: expval->bool [-> ExpVal Boolean])
  (define expval->bool (λ (val) (cast val Boolean)))

  (: expval->symbol [-> ExpVal Symbol])
  (define expval->symbol (λ (val) (cast val Symbol)))

  (: expval->char [-> ExpVal Char])
  (define expval->char (λ (val) (cast val Char)))

  (: expval->string [-> ExpVal String])
  (define expval->string (λ (val) (cast val String)))

  (: expval->pair [-> ExpVal (Pair DenVal DenVal)])
  (define expval->pair (λ (val) (cast val (Pair DenVal DenVal))))

  (: expval->list [-> ExpVal (Listof DenVal)])
  (define expval->list (λ (val) (cast val (Listof DenVal))))

  (: expval->proc [-> ExpVal Proc])
  (define expval->proc (λ (val) (cast val Proc)))


  (: expval->s-expval [-> ExpVal Any])
  (define expval->s-expval
    (λ (val)
      (cond [(symbol? val) val]
            [(integer? val) val]
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
