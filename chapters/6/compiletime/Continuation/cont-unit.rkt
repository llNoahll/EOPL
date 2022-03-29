#lang typed/racket

(require "../types/types.rkt"
         "../ExpValues/values-sig.rkt"
         "../Procedure/proc-sig.rkt"
         "cont-sig.rkt")

(provide cont@)


(define-unit cont@
  (import values^ proc^)
  (export cont^)

  (: id-cont [-> Null])
  (define id-cont
    (let ([cont '()])
      (λ () cont)))

  (: end-cont [-> Null])
  (define end-cont
    (let ([cont '()])
      (λ ()
        #;(displayln "End Program!")
        cont)))

  (: apply-cont [-> Cont ExpVal FinalAnswer])
  (define apply-cont
    (λ (cont val)
      (if (null? cont)
          (final-answer val)
          (((frame-func (car cont)) (cdr cont)) val))))

  )
