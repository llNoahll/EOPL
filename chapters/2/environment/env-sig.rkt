#lang racket


(define-signature env^
  (empty-env
   empty-env?
   extend-env
   extend-env*
   extend-env?
   env?
   apply-env
   has-binding?))

(provide env^)