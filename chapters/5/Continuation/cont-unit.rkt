#lang typed/racket

(require "../types/types.rkt"
         "cont-sig.rkt")

(provide cont@)


(define-unit cont@
  (import)
  (export cont^)

  (: id-cont [-> Cont])
  (define id-cont (λ () (λ (val) val)))

  (: end-cont [-> Cont])
  (define end-cont
    (λ ()
      (displayln "End of Computation!")
      (id-cont)))

  (: apply-cont [-> Cont ExpVal FinalAnswer])
  (define apply-cont
    (λ (cont val)
      (cont val)))

  )
