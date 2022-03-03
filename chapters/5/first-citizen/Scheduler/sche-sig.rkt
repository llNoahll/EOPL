#lang typed/racket

(require "../types/types.rkt")

(provide sche^)


(define-signature sche^
  (
   [initialize-scheduler!  : [-> Exact-Positive-Integer Void]]
   [place-on-thread-queue  : (case-> [-> (Queueof Thd)
                                         (U Thd [-> FinalAnswer])
                                         (Queueof Thd)]
                                     [-> (Queueof Thd)
                                         [-> FinalAnswer]
                                         Natural
                                         Natural
                                         (Queueof Thd)])]
   [place-on-ready-queue!  : (case-> [-> (U Thd [-> FinalAnswer]) Void]
                                     [-> [-> FinalAnswer] Natural Natural Void])]
   [run-next-thread        : [-> FinalAnswer]]
   [set-final-answer!      : [-> ExpVal Void]]
   [time-expired?          : [-> Boolean]]
   [decrement-timer!       : [-> Void]]
   ))
