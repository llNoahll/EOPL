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

  (: bytes-val [-> Bytes DenVal])
  (define bytes-val (λ (bs) bs))

  (: proc-val [-> Proc DenVal])
  (define proc-val (λ (proc) proc))

  (: primitive-proc-val [-> Primitive-Proc DenVal])
  (define primitive-proc-val (λ (primitive-proc) primitive-proc))

  (: queue-val [-> (Queueof DenVal) DenVal])
  (define queue-val (λ (queue) queue))

  (: box-val [-> (Boxof DenVal) DenVal])
  (define box-val (λ (bx) bx))

  (: pair-val [-> (Pairof DenVal DenVal) DenVal])
  (define pair-val (λ (pair) pair))

  (: list-val [-> (Listof DenVal) DenVal])
  (define list-val (λ (ls) ls))

  (: vector-val [-> (Vectorof DenVal) DenVal])
  (define vector-val (λ (vec) vec))

  (: hash-val [-> (HashTable DenVal DenVal) DenVal])
  (define hash-val (λ (ht) ht))


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

  (: expval->bytes [-> ExpVal Bytes])
  (define expval->bytes (λ (val) (assert val bytes?)))

  (: expval->proc [-> ExpVal Proc])
  (define expval->proc (λ (val) (assert val proc?)))

  (: expval->primitive-proc [-> ExpVal Primitive-Proc])
  (define expval->primitive-proc (λ (val) (assert val primitive-proc?)))

  (: expval->queue [-> ExpVal (Queueof DenVal)])
  (define expval->queue (λ (val) (assert val (queueof? denval?))))

  (: expval->box [-> ExpVal (Boxof DenVal)])
  (define expval->box (λ (val) (assert val denbox?)))

  (: expval->pair [-> ExpVal (Pairof DenVal DenVal)])
  (define expval->pair (λ (val) (assert val denpair?)))

  (: expval->list [-> ExpVal (Listof DenVal)])
  (define expval->list (λ (val) (assert val (listof? denval?))))

  (: expval->vector [-> ExpVal (Vectorof DenVal)])
  (define expval->vector (λ (val) (assert val denvector?)))

  (: expval->hash [-> ExpVal (HashTable DenVal DenVal)])
  (define expval->hash (λ (val) (assert val denhash?)))


  (: expval->denval [-> ExpVal DenVal])
  (define expval->denval (λ (val) (assert val denval?)))


  (: s-expval->denval [-> Any DenVal])
  (define s-expval->denval (λ (val) (assert val denval?)))

  (: s-expval->expval [-> Any ExpVal])
  (define s-expval->expval (λ (val) (assert val expval?)))

  )
