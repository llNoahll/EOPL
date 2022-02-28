#lang typed/racket

(require "../types/types.rkt"
         "../Scheduler/sche-sig.rkt"
         "mut-sig.rkt")

(provide mut@)


(define-unit mut@
  (import sche^)
  (export mut^)

  (: wait-for-mutex [-> Mutex Thd FinalAnswer])
  (define wait-for-mutex
    (λ (mut thd)
      (match mut
        [(mutex keys wait-queue)
         (cond [(zero? keys)
                (set-mutex-wait-queue! mut (enqueue wait-queue thd))
                (run-next-thread)]
               [else
                (set-mutex-keys! mut (sub1 keys))
                (final-answer (thd))])])))

  (: signal-mutex [-> Mutex Thd FinalAnswer])
  (define signal-mutex
    (λ (mut thd)
      (match mut
        [(mutex keys wait-queue)
         (if (empty-queue? wait-queue)
             (set-mutex-keys! mut (add1 keys))
             (dequeue wait-queue
                      (ann (λ (1st-waiting-thd other-waiting-thds)
                             (place-on-ready-queue! 1st-waiting-thd)
                             (set-mutex-wait-queue! mut other-waiting-thds))
                           [-> Thd (Queueof Thd) Void])))
         (final-answer (thd))])))

  )
