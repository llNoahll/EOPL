#lang typed/racket

(require "../types/types.rkt"
         "thd-sig.rkt")

(provide thd@)


(: ptid Natural)
(define ptid 0)

(: tid  Natural)
(define tid  1)

(: ntid Natural)
(define ntid 2)


(define-unit thd@
  (import)
  (export thd^)


  (: initialize-thread-identifier! [-> Void])
  (define initialize-thread-identifier!
    (λ ()
      (set! ptid 0)
      (set! tid  1)
      (set! ntid 2)))

  (: update-thread-identifier! [-> Thd Void])
  (define update-thread-identifier!
    (λ (th)
      (set! ptid (thd-ptid th))
      (set! tid  (thd-tid  th))))


  (: get-tid [-> Natural])
  (define get-tid (λ () tid))

  (: get-ptid [-> Natural])
  (define get-ptid (λ () ptid))

  (: get-nid [-> Natural])
  (define get-nid (λ () (begin0 ntid (set! ntid (add1 ntid)))))


  (: apply-thd [-> Thd FinalAnswer])
  (define apply-thd
    (λ (th)
      (update-thread-identifier! th)
      ((thd-thunk th))))

  )
