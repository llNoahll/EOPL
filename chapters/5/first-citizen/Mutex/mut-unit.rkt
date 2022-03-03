#lang typed/racket

(require "../types/types.rkt"
         "../Scheduler/sche-sig.rkt"
         "mut-sig.rkt")

(provide mut@)


(define-unit mut@
  (import sche^)
  (export mut^)

  (: wait-for-mutex [-> Mutex [-> FinalAnswer] FinalAnswer])
  (define wait-for-mutex
    (λ (mut thk)
      (match mut
        [(mutex keys wait-queue)
         (cond [(zero? keys)
                (set-mutex-wait-queue! mut (place-on-thread-queue wait-queue thk))
                (run-next-thread)]
               [else
                (set-mutex-keys! mut (sub1 keys))
                (thk)])])))

  (: signal-mutex [-> Mutex [-> FinalAnswer] FinalAnswer])
  (define signal-mutex
    (λ (mut thk)
      (match mut
        [(mutex keys wait-queue)
         (if (empty-queue? wait-queue)
             (set-mutex-keys! mut (add1 keys))
             (dequeue wait-queue
                      (ann (λ (1st-waiting-thd other-waiting-thds)
                             (place-on-ready-queue! 1st-waiting-thd)
                             (set-mutex-wait-queue! mut other-waiting-thds))
                           [-> Thd* (Queueof Thd*) Void])))
         (thk)])))

  )
