#lang typed/racket

(require "../types/types.rkt"
         "../Reference/ref-sig.rkt"
         "../Thread/thd-sig.rkt"
         "../Environment/env-sig.rkt"
         "sche-sig.rkt")

(provide sche@)


(: the-ready-queue Ref)
(define the-ready-queue (make-ref (empty-queue)))

(: the-final-answer Ref)
(define the-final-answer (make-ref (final-answer undefined)))

(: the-max-time-slice Ref)
(define the-max-time-slice (make-ref 1))

(: the-time-remaining Ref)
(define the-time-remaining (make-ref 0))


(define-unit sche@
  (import ref^ thd^ env^)
  (export sche^)

  (: thread-env [-> Env])
  (define thread-env
    (λ ()
      (extend-env-bind+
       `([the-ready-queue    . ,the-ready-queue]
         [the-final-answer   . ,the-final-answer]
         [the-max-time-slice . ,the-max-time-slice]
         [the-time-remaining . ,the-time-remaining])
       (base-env))))


  (: initialize-scheduler! [-> Exact-Positive-Integer Void])
  (define initialize-scheduler!
    (λ (ticks)
      (setref! the-ready-queue    (empty-queue))
      (setref! the-final-answer   (final-answer undefined))
      (setref! the-max-time-slice ticks)
      (setref! the-time-remaining (deref the-max-time-slice))))

  (: place-on-thread-queue
     (case-> [-> (Queueof Natural) (U Natural [-> FinalAnswer]) (Queueof Natural)]
             [-> (Queueof Natural)
                 [-> FinalAnswer] Natural Natural (Boxof (Queueof DenVal))
                 (Queueof Natural)]))
  (define place-on-thread-queue
    (case-lambda
      [(thds t)
       (if (natural? t)
           (enqueue thds t)
           (place-on-thread-queue thds t (get-ptid) (get-tid) (get-mail)))]
      [(thds thk ptid tid mail)
       (define th
         (thd ptid tid mail
              (let ([the-time (deref the-time-remaining)])
                (if (exact-positive-integer? the-time)
                    the-time
                    (assert (deref the-max-time-slice) exact-positive-integer?)))
              thk))
       (add-thread! tid th)
       (enqueue thds tid)]))

  (: place-on-ready-queue!
     (case-> [-> (U Natural [-> FinalAnswer]) Void]
             [-> [-> FinalAnswer] Natural Natural (Boxof (Queueof DenVal)) Void]))
  (define place-on-ready-queue!
    (case-lambda
      [(t)
       (setref! the-ready-queue (place-on-thread-queue (assert (deref the-ready-queue) (queueof? natural?)) t))]
      [(thk ptid tid mail)
       (setref! the-ready-queue (place-on-thread-queue (assert (deref the-ready-queue) (queueof? natural?)) thk ptid tid mail))]))

  (: run-next-thread [-> FinalAnswer])
  (define run-next-thread
    (λ ()
      (if (empty-queue? (deref the-ready-queue))
          (final-answer (deref the-final-answer))
          (dequeue (assert (deref the-ready-queue) (queueof? natural?))
                   (ann (λ (1st-ready-tid other-ready-tids)
                          (define th (get-thread 1st-ready-tid))

                          (setref! the-ready-queue other-ready-tids)
                          (cond [(boolean? th) (run-next-thread)]
                                [(thd? th)
                                 (setref! the-time-remaining (thd-time-slice th))
                                 (apply-thd th)]))
                        [-> Natural (Queueof Natural) FinalAnswer])))))

  (: set-final-answer! [-> ExpVal Void])
  (define set-final-answer!
    (λ (val)
      (setref! the-final-answer (final-answer val))))

  (: time-expired? [-> Boolean])
  (define time-expired?
    (λ ()
      (eq? 0 (deref the-time-remaining))))

  (: decrement-timer! [-> Void])
  (define decrement-timer!
    (λ ()
      (setref! the-time-remaining (sub1 (assert (deref the-time-remaining) exact-positive-integer?)))))

  )
