#lang racket


(define-signature lambda-expression^
  (
   ;; constructors
   var-exp
   lambda-exp
   app-exp

   ;; predicates
   var-exp?
   lambda-exp?
   app-exp?

   ;; extractors
   var-exp->var
   lambda-exp->bound-var
   lambda-exp->body
   app-exp->rator
   app-exp->rand
   ))

(provide lambda-expression^)