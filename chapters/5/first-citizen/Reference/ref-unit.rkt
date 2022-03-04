#lang typed/racket

(require "../types/types.rkt"
         "ref-sig.rkt")

(provide ref@)


(define-unit ref@
  (import)
  (export ref^)

  (: newref [-> DenVal Ref])
  (define newref (λ (val) (make-ref val)))

  (: deref [-> Ref DenVal])
  (define deref (λ (ref) (ref-val ref)))

  (: setref! [-> Ref DenVal Void])
  (define setref! (λ (ref val) (set-ref-val! ref val)))

  (: ref? [-> Any Boolean : Ref])
  (define-predicate ref? Ref)

  )
