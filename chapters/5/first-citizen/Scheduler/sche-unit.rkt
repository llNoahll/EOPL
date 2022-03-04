#lang typed/racket

(require "../types/types.rkt"
         "../Thread/thd-sig.rkt"
         "sche-sig.rkt")

(provide sche@)


(: the-ready-queue (Queueof Natural))
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
     (case-> [-> (Queueof Natural) (U Natural [-> FinalAnswer]) (Queueof Natural)]
             [-> (Queueof Natural) [-> FinalAnswer] Natural Natural (Queueof Natural)]))
  (define place-on-thread-queue
    (case-lambda
      [(thds t)
       (if (natural? t)
           (enqueue thds t)
           (place-on-thread-queue thds t (get-ptid) (get-tid)))]
      [(thds thk ptid tid)
       (define th
         (thd ptid tid (box (empty-queue))
              (let ([the-time the-time-remaining])
                (if (exact-positive-integer? the-time)
                    the-time
                    the-max-time-slice))
              thk))
       (add-thread! tid th)
       (enqueue thds tid)]))

  (: place-on-ready-queue!
     (case-> [-> (U Natural [-> FinalAnswer]) Void]
             [-> [-> FinalAnswer] Natural Natural Void]))
  (define place-on-ready-queue!
    (case-lambda
      [(t)
       (set! the-ready-queue (place-on-thread-queue the-ready-queue t))]
      [(thk ptid tid)
       (set! the-ready-queue (place-on-thread-queue the-ready-queue thk ptid tid))]))

  (: run-next-thread [-> FinalAnswer])
  (define run-next-thread
    (λ ()
      (if (empty-queue? the-ready-queue)
          the-final-answer
          (dequeue the-ready-queue
                   (ann (λ (1st-ready-tid other-ready-tids)
                          (define th (get-thread 1st-ready-tid))

                          (set! the-ready-queue other-ready-tids)
                          (cond [(boolean? th) (run-next-thread)]
                                [(thd? th)
                                 (set! the-time-remaining (thd-time-slice th))
                                 (apply-thd th)]))
                        [-> Natural (Queueof Natural) FinalAnswer])))))

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
