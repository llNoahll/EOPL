#lang typed/racket

(provide (all-defined-out))


(define-type DenVal (U Symbol Integer Boolean (Pair DenVal DenVal) Null))
(define-type ExpVal (U DenVal Void Nothing))
