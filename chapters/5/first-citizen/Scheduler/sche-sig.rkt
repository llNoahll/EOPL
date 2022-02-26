#lang typed/racket

(require "../types/types.rkt")

(provide sche^)


(define-signature sche^
  (
   [initialize-scheduler! : [-> Exact-Positive-Integer Void]]
   [place-on-ready-queue! : [-> Thd Void]]
   [run-next-thread       : [-> FinalAnswer]]
   [set-final-answer!     : [-> ExpVal Void]]
   [time-expired?         : [-> Boolean]]
   [decrement-timer!      : [-> Void]]
   ))