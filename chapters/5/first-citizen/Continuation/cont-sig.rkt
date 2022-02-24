#lang typed/racket

(require "../types/types.rkt")

(provide cont^)


(define-signature cont^
  (
   [id-cont*   : [-> Id-Cont*]]
   [end-cont*  : [-> End-Cont*]]
   [apply-cont : [-> Cont* ExpVal FinalAnswer]]
   ))
