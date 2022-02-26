#lang typed/racket

(require "../types/types.rkt"
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
  (import)
  (export sche^)

  (: initialize-scheduler! [-> Exact-Positive-Integer Void])
  (define initialize-scheduler!
    (λ (ticks)
      (set! the-ready-queue (empty-queue))
      (set! the-final-answer (final-answer undefined))
      (set! the-max-time-slice ticks)
      (set! the-time-remaining the-max-time-slice)))

  (: place-on-ready-queue! [-> Thd Void])
  (define place-on-ready-queue!
    (λ (th)
      (set! the-ready-queue
            (enqueue the-ready-queue th))))

  (: run-next-thread [-> FinalAnswer])
  (define run-next-thread
    (λ ()
      (if (empty-queue? the-ready-queue)
          the-final-answer
          (dequeue the-ready-queue
                   (ann (λ (1st-read-thd other-ready-thds)
                          (set! the-ready-queue other-ready-thds)
                          (set! the-time-remaining the-max-time-slice)
                          (final-answer (1st-read-thd)))
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
