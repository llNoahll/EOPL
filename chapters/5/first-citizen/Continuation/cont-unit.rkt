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

  (: id-cont [-> Null])
  (define id-cont
    (let ([cont '()])
      (λ () cont)))

  (: end-cont [-> Null])
  (define end-cont
    (let ([cont '()])
      (λ () cont)))

  (: end-subthread-cont [-> (List Frame)])
  (define end-subthread-cont
    (let ([cont
           (list
            (frame
             'end-subthread-frame
             #f
             (ann (λ (cont)
                    (λ (val)
                      #;(displayln "End subthread!")
                      (run-next-thread)))
                  [-> Cont [-> ExpVal FinalAnswer]])))])
      (λ () cont)))

  (: end-main-thread-cont [-> (List Frame)])
  (define end-main-thread-cont
    (let ([cont
           (list
            (frame
             'end-main-thread-frame
             #f
             (ann (λ (cont)
                    (λ (val)
                      (set-final-answer! (final-answer val))
                      #;(displayln "End main thread!")
                      (run-next-thread)))
                  [-> Cont [-> ExpVal FinalAnswer]])))])
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
             (if (null? cont)
                 (final-answer val)
                 (((frame-func (car cont)) (cdr cont)) val))])))

  (: apply-handler [-> Cont DenVal FinalAnswer])
  (define apply-handler
    (λ (cont ex)
      (: args (Listof DenVal))
      (define args (list ex))

      (let loop : FinalAnswer
           ([handlers-cont (inherit-handlers-cont cont)])
        (define handlers-frame
          (if (handlers-cont? handlers-cont)
              (car handlers-cont)
              (error "uncaught exception: " ex)))

        (let check : FinalAnswer
             ([preds    (handlers-frame-preds    handlers-frame)]
              [handlers (handlers-frame-handlers handlers-frame)])
          (if (or (null? preds) (null? handlers))
              (loop (frame-handlers-cont handlers-frame))
              (apply-procedure/k
               (car preds) args
               (cons (frame 'raise-frame
                            (frame-handlers-cont handlers-frame)
                            (ann (λ (cont)
                                   (λ (val)
                                     (if (expval->bool val)
                                         (apply-procedure/k (car handlers) args cont)
                                         (check (cdr preds) (cdr handlers)))))
                                 [-> Cont [-> ExpVal FinalAnswer]]))
                     handlers-cont)))))))

  (: inherit-handlers-cont [-> Cont (Option Handlers-Cont)])
  (define inherit-handlers-cont
    (λ (cont)
      (if (null? cont)
          #f
          (if (handlers-cont? cont)
              cont
              (frame-handlers-cont (car cont))))))

  )
