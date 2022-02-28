#lang typed/racket

(require "../types/types.rkt")

(provide mut^)


(define-signature mut^
  (
   [wait-for-mutex : [-> Mutex Thd FinalAnswer]]
   [signal-mutex   : [-> Mutex Thd FinalAnswer]]
   ))
