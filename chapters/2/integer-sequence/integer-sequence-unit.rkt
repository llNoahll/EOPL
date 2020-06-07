#lang typed/racket


(require/typed "integer-sequence-sig.rkt"
  [#:signature integer-sequence^
   (
    [number->sequence : [-> Integer NodeInSequence]]
    [current-element  : [-> NodeInSequence Integer]]

    [move-to-left  : [-> NodeInSequence NodeInSequence]]
    [move-to-right : [-> NodeInSequence NodeInSequence]]

    [insert-to-left  : [-> Integer NodeInSequence NodeInSequence]]
    [insert-to-right : [-> Integer NodeInSequence NodeInSequence]]

    [at-left-end?  : [-> NodeInSequence Boolean]]
    [at-right-end? : [-> NodeInSequence Boolean]]
    )])

(provide integer-sequence@ NodeInSequence)


(define-type NodeInSequence (List Integer (Listof Integer) (Listof Integer)))


(define-unit integer-sequence@
  (import)
  (export integer-sequence^)


  (: number->sequence [-> Integer NodeInSequence])
  (define number->sequence
    (λ (num)
      (list num '() '())))

  (: current-element [-> NodeInSequence Integer])
  (define current-element
    (λ (seq)
      (car seq)))


  (: move-to-left [-> NodeInSequence NodeInSequence])
  (define move-to-left
    (λ (seq)
      (if (at-left-end? seq)
          (raise-argument-error 'move-to-left "not-at-left-end?" seq)
          (match seq
            [(list current-num
                   (list right-num right-nums ...)
                   (list left-nums ...))
             (list right-num
                   right-nums
                   (cons current-num left-nums))]))))

  (: move-to-right [-> NodeInSequence NodeInSequence])
  (define move-to-right
    (λ (seq)
      (if (at-right-end? seq)
          (raise-argument-error 'move-to-right "not-at-right-end?" seq)
          (match seq
            [(list current-num
                   (list right-nums ...)
                   (list left-num left-nums ...))
             (list left-num
                   (cons current-num right-nums)
                   left-nums)]))))


  (: insert-to-left [-> Integer NodeInSequence NodeInSequence])
  (define insert-to-left
    (λ (num seq)
      (match seq
        [(list current-num left-nums right-nums)
         (list current-num (cons num left-nums) right-nums)])))

  (: insert-to-right [-> Integer NodeInSequence NodeInSequence])
  (define insert-to-right
    (λ (num seq)
      (match seq
        [(list current-num left-nums right-nums)
         (list current-num left-nums (cons num right-nums))])))


  (: at-left-end? [-> NodeInSequence Boolean])
  (define at-left-end?
    (λ (seq)
      (match seq
        [`(_ ,(? null?) _) #t]
        [_ #f])))

  (: at-right-end? [-> NodeInSequence Boolean])
  (define at-right-end?
    (λ (seq)
      (match seq
        [`(_ _ ,(? null?)) #t]
        [_ #f]))))
