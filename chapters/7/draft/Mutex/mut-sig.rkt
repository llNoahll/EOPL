#lang typed/racket

(require "../types/types.rkt")

(provide mut^)


(define-signature mut^
  (
   [wait-for-mutex : [-> Mutex [-> FinalAnswer] FinalAnswer]]
   [signal-mutex   : [-> Mutex [-> FinalAnswer] FinalAnswer]]
   ))
