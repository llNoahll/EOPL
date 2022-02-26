#lang typed/racket

(require "../types/types.rkt"
         "../Scheduler/sche-sig.rkt"
         "../ExpValues/values-sig.rkt"
         "../Procedure/proc-sig.rkt"
         "cont-sig.rkt")

(provide cont@)


(define-unit cont@
  (import sche^ values^ proc^)
  (export cont^)

  (: id-cont* [-> Id-Cont*])
  (define id-cont*
    (let ([cont (id-cont 'id-cont #f final-answer)])
      (λ () cont)))

  (: end-cont* [-> End-Cont*])
  (define end-cont*
    (let ([cont (end-cont 'end-cont #f
                          (ann (λ (val)
                                 #;(displayln "End of Computation!")
                                 (final-answer val))
                               [-> ExpVal FinalAnswer]))])
      (λ () cont)))

  (: end-subthread-cont* [-> End-Cont*])
  (define end-subthread-cont*
    (let ([cont (end-cont 'end-subthread-cont #f
                          (ann (λ (val)
                                 #;(displayln "End subthread!")
                                 (run-next-thread))
                               [-> ExpVal FinalAnswer]))])
      (λ () cont)))

  (: end-main-thread-cont* [-> End-Cont*])
  (define end-main-thread-cont*
    (let ([cont (end-cont 'end-main-thread-cont #f
                          (ann (λ (val)
                                 (set-final-answer! (final-answer val))
                                 #;(displayln "End main thread!")
                                 (run-next-thread))
                               [-> ExpVal FinalAnswer]))])
      (λ () cont)))

  (: apply-cont [-> Cont ExpVal FinalAnswer])
  (define apply-cont
    (λ (cont val)
      (cond [(time-expired?)
             (place-on-ready-queue!
              (λ () (apply-cont cont val)))
             (run-next-thread)]
            [else
             (decrement-timer!)
             ((cont-func cont) val)])))

  (: apply-handler [-> Cont* DenVal FinalAnswer])
  (define apply-handler
    (λ (cont* ex)
      (: args (Listof DenVal))
      (define args (list ex))

      (let loop : FinalAnswer ([handlers-cont* (inherit-handlers-cont* cont*)])
        (if (handlers-cont? handlers-cont*)
            (let check : FinalAnswer
                 ([preds    (handlers-cont-preds    handlers-cont*)]
                  [handlers (handlers-cont-handlers handlers-cont*)])
              (if (or (null? preds) (null? handlers))
                  (loop (cont-handlers-cont* handlers-cont*))
                  (apply-procedure/k
                   (car preds) args
                   (cont 'raise-cont
                         (cont-handlers-cont* handlers-cont*)
                         (ann (λ (val)
                                (if (expval->bool val)
                                    (apply-procedure/k (car handlers) args handlers-cont*)
                                    (check (cdr preds) (cdr handlers))))
                              [-> ExpVal FinalAnswer])))))
            (error "uncaught exception: " ex)))))

  (: inherit-handlers-cont* [-> Cont* (Option Handlers-Cont*)])
  (define inherit-handlers-cont*
    (λ (cont*)
      (if (handlers-cont? cont*)
          cont*
          (cont-handlers-cont* cont*))))

  )
