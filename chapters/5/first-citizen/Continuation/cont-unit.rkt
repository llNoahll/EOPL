#lang typed/racket

(require "../types/types.rkt"
         "cont-sig.rkt")

(provide cont@)


(define-unit cont@
  (import)
  (export cont^)

  (: id-cont* [-> Id-Cont*])
  (define id-cont* (λ () (id-cont 'id-cont id-cont* final-answer)))

  (: end-cont* [-> End-Cont*])
  (define end-cont*
    (λ ()
      (end-cont 'end-cont end-cont*
                (ann (λ (val)
                       (displayln "End of Computation!")
                       (final-answer val))
                     [-> ExpVal FinalAnswer]))))

  (: apply-cont [-> Cont* ExpVal FinalAnswer])
  (define apply-cont (λ (cont* val) ((cont-func cont*) val)))

  )
