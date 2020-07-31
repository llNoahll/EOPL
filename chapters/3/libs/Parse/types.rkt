#lang typed/racket

(provide (all-defined-out))


(define-type S-List (Listof S-Exp))
(define-type S-Exp (U Boolean Integer Symbol S-List))

(define-predicate s-exp?  S-Exp)
(define-predicate s-list? S-List)
