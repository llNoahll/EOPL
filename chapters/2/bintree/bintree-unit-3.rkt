#lang racket


(require "bintree-sig.rkt")

(provide bintree@)


(define-unit bintree@
  (import)
  (export bintree^)


  (define number->bintree
    (λ (num)
      (stream num empty-stream
              empty-stream
              empty-stream)))

  (define current-element
    (λ (btree)
      (if (stream-empty? btree)
          (raise-argument-error 'current-element "not-at-leaf?" btree)
          (stream-ref btree 0))))


  (define move-up
    (λ (btree)
      (if (stream-empty? btree)
          (raise-argument-error 'move-to-left-son "not-at-leaf?" btree)
          (stream-ref btree 1))))

  (define move-to-left-son
    (λ (btree)
      (if (stream-empty? btree)
          (raise-argument-error 'move-to-left-son "not-at-leaf?" btree)
          (stream-ref btree 2))))

  (define move-to-right-son
    (λ (btree)
      (if (stream-empty? btree)
          (raise-argument-error 'move-to-left-son "not-at-leaf?" btree)
          (stream-ref btree 3))))


  (define change-father
    (λ (btree new-father)
      (match (stream->list btree)
        [(? null?) empty-stream]
        [(list root father left-son right-son)
         (letrec ([new-btree (stream root new-father
                                     (change-father left-son  new-btree)
                                     (change-father right-son new-btree))])
           new-btree)]
        [_ (raise-argument-error 'change-father "btree?" btree)])))


  (define insert-to-left
    (λ (num btree)
      (if (stream-empty? btree)
          (raise-argument-error 'insert-to-left "not-at-leaf?" btree)
          (let* ([root (current-element btree)]
                 [father (move-up btree)]
                 [left-son  (move-to-left-son btree)]
                 [right-son (move-to-right-son btree)])
            (letrec ([new-btree
                      (stream root father
                              (letrec ([new-btree-lson
                                        (stream num new-btree
                                                (change-father left-son new-btree-lson)
                                                empty-stream)])
                                new-btree-lson)
                              (change-father right-son new-btree))])
              new-btree)))))

  (define insert-to-right
    (λ (num btree)
      (if (stream-empty? btree)
          (raise-argument-error 'insert-to-left "not-at-leaf?" btree)
          (let* ([root (current-element btree)]
                 [father (move-up btree)]
                 [left-son  (move-to-left-son btree)]
                 [right-son (move-to-right-son btree)])
            (letrec ([new-btree
                      (stream root father
                              (change-father left-son new-btree)
                              (letrec ([new-btree-rson
                                        (stream num new-btree
                                                (change-father right-son new-btree-rson)
                                                empty-stream)])
                                new-btree-rson))])
              new-btree)))))


  (define at-root?
    (λ (btree)
      (stream-empty? (move-up btree))))

  (define at-leaf?
    (λ (btree)
      (stream-empty? btree))))
