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

  )
