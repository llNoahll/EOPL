#lang typed/racket

(require "../Environment/types.rkt"
         "../Expressions/types.rkt")

(provide (all-defined-out))


(define-struct proc
  ([vars : (Listof Symbol)]
   [body : Exp]
   [saved-env : Env])
  #:transparent
  #:type-name Proc)
