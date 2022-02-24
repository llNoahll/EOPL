#lang typed/racket

(require "../types/types.rkt"
         "../ExpValues/values-sig.rkt"
         "../Procedure/proc-sig.rkt"
         "cont-sig.rkt")

(provide cont@)


(define-unit cont@
  (import values^ proc^)
  (export cont^)

  (: id-cont* [-> Id-Cont*])
  (define id-cont* (λ () (id-cont 'id-cont #f final-answer)))

  (: end-cont* [-> End-Cont*])
  (define end-cont*
    (λ ()
      (end-cont 'end-cont #f
                (ann (λ (val)
                       (displayln "End of Computation!")
                       (final-answer val))
                     [-> ExpVal FinalAnswer]))))

  (: apply-cont [-> Cont* ExpVal FinalAnswer])
  (define apply-cont (λ (cont* val) ((cont-func cont*) val)))

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
