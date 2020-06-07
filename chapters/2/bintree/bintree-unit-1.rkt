#lang typed/racket


(require/typed "bintree-sig.rkt"
  [#:signature bintree^
   (
    [number->bintree : [-> Integer BinTree]]
    [current-element : [-> BinTree Integer]]

    [move-to-left-son  : [-> BinTree BinTree]]
    [move-to-right-son : [-> BinTree BinTree]]

    [insert-to-left  : [-> Integer BinTree BinTree]]
    [insert-to-right : [-> Integer BinTree BinTree]]

    [at-leaf? : [-> BinTree Boolean]]
    )])

(provide bintree@ BinTree)


(define-type BinTree (U Null (List Integer BinTree BinTree)))


(define-unit bintree@
  (import)
  (export bintree^)


  (: number->bintree [-> Integer BinTree])
  (define number->bintree
    (λ (num)
      (list num '() '())))

  (: current-element [-> BinTree Integer])
  (define current-element
    (λ (btree)
      (if (null? btree)
          (raise-argument-error 'current-element "not-at-leaf?" btree)
          (car btree))))


  (: move-to-left-son [-> BinTree BinTree])
  (define move-to-left-son
    (λ (btree)
      (if (null? btree)
          (raise-argument-error 'move-to-left-son "not-at-leaf?" btree)
          (cadr btree))))

  (: move-to-right-son [-> BinTree BinTree])
  (define move-to-right-son
    (λ (btree)
      (if (null? btree)
          (raise-argument-error 'move-to-left-son "not-at-leaf?" btree)
          (caddr btree))))


  (: insert-to-left [-> Integer BinTree BinTree])
  (define insert-to-left
    (λ (num btree)
      (match btree
        [(? null?) (raise-argument-error 'insert-to-left "not-at-leaf?" btree)]
        [`(,root ,left-son ,right-son)
         (list root
               (list num left-son '())
               right-son)])))

  (: insert-to-right [-> Integer BinTree BinTree])
  (define insert-to-right
    (λ (num btree)
      (match btree
        [(? null?) (raise-argument-error 'insert-to-right "not-at-leaf?" btree)]
        [`(,root ,left-son ,right-son)
         (list root
               left-son
               (list num right-son '()))])))


  (: at-leaf? [-> BinTree Boolean])
  (define at-leaf?
    (λ (btree)
      (null? btree))))
