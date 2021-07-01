#lang typed/racket

(require "../types/types.rkt")

(provide primitive-proc^)


(define-signature primitive-proc^
  (
   [base-env : (Parameter Env)]
   [primitive-proc-table : (Mutable-HashTable Symbol [-> DenVal * ExpVal])]
   ))
