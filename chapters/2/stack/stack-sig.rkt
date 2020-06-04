#lang racket

(provide stack^)


(define-signature stack^
  (empty-stack
   empty-stack?
   push!
   pop!
   top))
