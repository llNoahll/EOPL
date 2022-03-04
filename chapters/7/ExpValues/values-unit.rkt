#lang typed/racket

(require "../types/types.rkt"
         "values-sig.rkt")

(provide values@)


(define-unit values@
  (import)
  (export values^)

  (: symbol-val [-> Symbol DenVal])
  (define symbol-val (λ (symbol) symbol))

  (: num-val [-> Real DenVal])
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

  (: mutex-val [-> Mutex DenVal])
  (define mutex-val (λ (mut) mut))


  (: expval->num [-> ExpVal Real])
  (define expval->num (λ (val) (assert val real?)))

  (: expval->bool [-> ExpVal Boolean])
  (define expval->bool (λ (val) (assert val boolean?)))

  (: expval->symbol [-> ExpVal Symbol])
  (define expval->symbol (λ (val) (assert val symbol?)))

  (: expval->char [-> ExpVal Char])
  (define expval->char (λ (val) (assert val char?)))

  (: expval->string [-> ExpVal String])
  (define expval->string (λ (val) (assert val string?)))

  (: expval->pair [-> ExpVal (Pair DenVal DenVal)])
  (define expval->pair (λ (val) (assert val denpair?)))

  (: expval->list [-> ExpVal (Listof DenVal)])
  (define expval->list (λ (val) (assert val denlist?)))

  (: expval->proc [-> ExpVal Proc])
  (define expval->proc (λ (val) (assert val proc?)))

  (: expval->mutex [-> ExpVal Mutex])
  (define expval->mutex (λ (val) (assert val mutex?)))


  (: expval->denval [-> ExpVal DenVal])
  (define expval->denval (λ (val) (assert val denval?)))


  (: s-expval->expval [-> Any ExpVal])
  (define s-expval->expval (λ (val) (assert val expval?)))

  )
