#lang racket

(require "../types/types.rkt")

(provide proc^)


(define-signature proc^
  (
   proc?
   procedure
   apply-procedure
   free-vars
   ))
