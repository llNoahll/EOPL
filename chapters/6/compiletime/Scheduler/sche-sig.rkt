#lang typed/racket

(require "../types/types.rkt")

(provide sche^)


(define-signature sche^
  (
   [initialize-scheduler!  : [-> Exact-Positive-Integer Void]]
   [place-on-thread-queue  : (case-> [-> (Queueof Natural) (U Natural [-> FinalAnswer]) (Queueof Natural)]
                                     [-> (Queueof Natural)
                                         [-> FinalAnswer] Natural Natural (Boxof (Queueof DenVal))
                                         (Queueof Natural)])]
   [place-on-ready-queue!  : (case-> [-> (U Natural [-> FinalAnswer]) Void]
                                     [-> [-> FinalAnswer] Natural Natural (Boxof (Queueof DenVal)) Void])]
   [run-next-thread        : [-> FinalAnswer]]
   [set-final-answer!      : [-> ExpVal Void]]
   [time-expired?          : [-> Boolean]]
   [decrement-timer!       : [-> Void]]
   ))
