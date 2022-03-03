#lang typed/racket

(require "../types/types.rkt"
         "../Thread/thd-sig.rkt"
         "sche-sig.rkt")

(provide sche@)


(: the-ready-queue (Queueof Thd))
(define the-ready-queue (empty-queue))

(: the-final-answer FinalAnswer)
(define the-final-answer (final-answer undefined))

(: the-max-time-slice Exact-Positive-Integer)
(define the-max-time-slice 1)

(: the-time-remaining Natural)
(define the-time-remaining 0)


(define-unit sche@
  (import thd^)
  (export sche^)

  (: initialize-scheduler! [-> Exact-Positive-Integer Void])
  (define initialize-scheduler!
    (λ (ticks)
      (set! the-ready-queue (empty-queue))
      (set! the-final-answer (final-answer undefined))
      (set! the-max-time-slice ticks)
      (set! the-time-remaining the-max-time-slice)))

  (: place-on-thread-queue
     (case-> [-> (Queueof Thd) (U Thd [-> FinalAnswer]) (Queueof Thd)]
             [-> (Queueof Thd) [-> FinalAnswer] Natural Natural (Queueof Thd)]))
  (define place-on-thread-queue
    (case-lambda
      [(thds th)
       (if (thd? th)
           (enqueue thds th)
           (place-on-thread-queue thds th (get-ptid) (get-tid)))]
      [(thds thk ptid tid)
       (enqueue thds
                (thd ptid
                     tid
                     (let ([the-time the-time-remaining])
                       (if (exact-positive-integer? the-time)
                           the-time
                           the-max-time-slice))
                     thk))]))

  (: place-on-ready-queue!
     (case-> [-> (U Thd [-> FinalAnswer]) Void]
             [-> [-> FinalAnswer] Natural Natural Void]))
  (define place-on-ready-queue!
    (case-lambda
      [(th)
       (set! the-ready-queue (place-on-thread-queue the-ready-queue th))]
      [(thk ptid tid)
       (set! the-ready-queue (place-on-thread-queue the-ready-queue thk ptid tid))]))

  (: run-next-thread [-> FinalAnswer])
  (define run-next-thread
    (λ ()
      (if (empty-queue? the-ready-queue)
          the-final-answer
          (dequeue the-ready-queue
                   (ann (λ (1st-ready-thd other-ready-thds)
                          (set! the-ready-queue other-ready-thds)
                          (set! the-time-remaining (thd-time-slice 1st-ready-thd))
                          (apply-thd 1st-ready-thd))
                        [-> Thd (Queueof Thd) FinalAnswer])))))

  (: set-final-answer! [-> ExpVal Void])
  (define set-final-answer!
    (λ (val)
      (set! the-final-answer
            (final-answer val))))

  (: time-expired? [-> Boolean])
  (define time-expired?
    (λ ()
      (zero? the-time-remaining)))

  (: decrement-timer! [-> Void])
  (define decrement-timer!
    (λ ()
      (set! the-time-remaining
            (assert (sub1 the-time-remaining) natural?))))

  )
