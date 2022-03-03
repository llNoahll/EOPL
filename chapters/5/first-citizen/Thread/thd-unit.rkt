#lang typed/racket

(require "../types/types.rkt"
         "thd-sig.rkt")

(provide thd@)


(: thread-table (Mutable-HashTable Natural (U True Thd)))
(define thread-table (make-hasheq '([0 . #t] [1 . #t])))


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
      (hash-clear! thread-table)
      (hash-set*! thread-table 0 #t 1 #t)
      (set! ptid 0)
      (set! tid  1)
      (set! ntid 2)))

  (: update-thread-identifier! [-> Thd Void])
  (define update-thread-identifier!
    (λ (th)
      (set! ptid (thd-ptid th))
      (set! tid  (thd-tid  th))
      (hash-set! thread-table tid #t)))

  (: kill-thread! [-> Natural (U Void Boolean)])
  (define kill-thread!
    (λ (tid)
      (define th (get-thread tid))
      (cond [(false? th) #f]
            [else
             (hash-remove! thread-table tid)
             (if (thd? th) #t (void))])))


  (: get-ptid [-> Natural])
  (define get-ptid (λ () ptid))

  (: get-tid [-> Natural])
  (define get-tid (λ () tid))

  (: get-nid [-> Natural])
  (define get-nid (λ () (begin0 ntid (set! ntid (add1 ntid)))))


  (: get-thread [-> Natural (U Boolean Thd)])
  (define get-thread
    (λ (tid)
      (hash-ref thread-table tid #f)))

  (: add-thread! [-> Natural Thd Void])
  (define add-thread!
    (λ (tid th)
      (hash-set! thread-table tid th)))


  (: apply-thd [-> Thd FinalAnswer])
  (define apply-thd
    (λ (th)
      (update-thread-identifier! th)
      ((thd-thunk th))))

  )
