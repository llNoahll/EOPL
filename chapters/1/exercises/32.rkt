#lang typed/racket

(require "31.rkt")


(: double-tree [-> BinTree BinTree])
(define double-tree
  (λ (btree)
    (if (leaf? btree)
        (leaf (* 2 (contents-of btree)))
        (interior-node (contents-of btree)
                       (double-tree (lson btree))
                       (double-tree (rson btree))))))


(displayln (double-tree
            (interior-node 'red
                           (interior-node 'bar
                                          (leaf 26)
                                          (leaf 12))
                           (interior-node 'red
                                          (leaf 11)
                                          (interior-node 'quux
                                                         (leaf 117)
                                                         (leaf 14))))))