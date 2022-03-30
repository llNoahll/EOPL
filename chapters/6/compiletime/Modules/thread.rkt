#lang typed/racket

(require "../types/types.rkt")

(provide module/thread)


(: module/thread [->* (S-Exp) (Exact-Positive-Integer) S-Exp])
(define module/thread
  (λ (code [timeslice 1])
    `(begin
       (initialize-scheduler! ,timeslice)
       (initialize-thread-identifier!)
       (spawn (λ (_) ,code))
       (run-next-thread))))
