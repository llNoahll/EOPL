#lang typed/racket

(require "../types/types.rkt"
         "sche-sig.rkt")

(provide sche@)


(: the-ready-queue (Queueof Thd*))
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

  (: place-on-thread-queue [-> (Queueof Thd*) [-> FinalAnswer] (Queueof Thd*)])
  (define place-on-thread-queue
    (λ (thds thk)
      (enqueue thds
               (thd (if (exact-positive-integer? the-time-remaining)
                        (assert the-time-remaining exact-positive-integer?)
                        the-max-time-slice)
                    (if (thd? thk)
                        (thd-thunk thk)
                        thk)))))

  (: place-on-ready-queue! [-> [-> FinalAnswer] Void])
  (define place-on-ready-queue!
    (λ (thk)
      (set! the-ready-queue
            (place-on-thread-queue the-ready-queue thk))))

  (: run-next-thread [-> FinalAnswer])
  (define run-next-thread
    (λ ()
      (if (empty-queue? the-ready-queue)
          the-final-answer
          (dequeue the-ready-queue
                   (ann (λ (1st-ready-thd other-ready-thds)
                          (set! the-ready-queue other-ready-thds)
                          (set! the-time-remaining (thd-time-slice 1st-ready-thd))
                          (final-answer (1st-ready-thd)))
                        [-> Thd* (Queueof Thd*) FinalAnswer])))))

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
