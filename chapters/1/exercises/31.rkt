#lang typed/racket

(provide (all-defined-out))


(define-type BinTree (U Leaf Interior-Node))
(define-type Leaf Integer)
(define-type Interior-Node (List Symbol BinTree BinTree))

(define-predicate leaf? Leaf)


(: leaf [-> Integer Leaf])
(define leaf (λ (content) content))

(: interior-node [-> Symbol BinTree BinTree Interior-Node])
(define interior-node
  (λ (content lnode rnode)
    (list content lnode rnode)))


(: lson [-> BinTree BinTree])
(define lson
  (λ (btree)
    (if (leaf? btree)
        (error 'lson "~s is not a binary tree" btree)
        (cadr btree))))

(: rson [-> BinTree BinTree])
(define rson
  (λ (btree)
    (if (leaf? btree)
        (error 'rson "~s is not a binary tree" btree)
        (caddr btree))))


(: contents-of (case-> [-> Leaf Integer]
                       [-> Interior-Node Symbol]))
(define contents-of
  (λ (btree)
    (if (leaf? btree)
        btree
        (car btree))))


;; (displayln (interior-node 'red
;;                           (interior-node 'bar
;;                                          (leaf 26)
;;                                          (leaf 12))
;;                           (interior-node 'red
;;                                          (leaf 11)
;;                                          (interior-node 'quux
;;                                                         (leaf 117)
;;                                                         (leaf 14)))))