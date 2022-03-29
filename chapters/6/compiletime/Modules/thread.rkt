#lang typed/racket

(require "../types/types.rkt")

(provide module/thread)


(: module/thread [->* (S-Exp) (Exact-Positive-Integer) S-Exp])
(define module/thread
  (λ (code [timeslice 1])
    `(begin
       (let/cc exit
         (let ([*apply* apply])
           (define apply
             (λ (func args)
               (let/cc return
                 (cond [(time-expired?)
                        (place-on-ready-queue!
                         (λ () (return (apply func args)))
                         (get-ptid) (get-tid) (get-mail))
                        (exit (run-next-thread))]
                       [else
                        (decrement-timer!)
                        (return (*apply* func args))]))))

           (initialize-scheduler! ,timeslice)
           (initialize-thread-identifier!)
           (apply (λ () (spawn (λ (_) ,code))) '())
           (run-next-thread))))

    ))
