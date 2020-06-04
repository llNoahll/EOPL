#lang typed/racket

(require "31.rkt")


(: mark-leaves-with-red-depth [-> BinTree BinTree])
(define mark-leaves-with-red-depth
  (λ (btree)
    (: mark-leaves-with-red-depth-iter [-> BinTree Natural BinTree])
    (define mark-leaves-with-red-depth-iter
      (λ (btree depth)
        (cond
          [(leaf? btree) (leaf depth)]
          [(symbol=? 'red (contents-of btree))
           (interior-node
            'red
            (mark-leaves-with-red-depth-iter (lson btree) (add1 depth))
            (mark-leaves-with-red-depth-iter (rson btree) (add1 depth)))]
          [else (interior-node
                 (contents-of btree)
                 (mark-leaves-with-red-depth-iter (lson btree) depth)
                 (mark-leaves-with-red-depth-iter (rson btree) depth))])))

    (mark-leaves-with-red-depth-iter btree 0)))


(displayln (mark-leaves-with-red-depth
            (interior-node 'red
                           (interior-node 'bar
                                          (leaf 26)
                                          (leaf 12))
                           (interior-node 'red
                                          (leaf 11)
                                          (interior-node 'quux
                                                         (leaf 117)
                                                         (leaf 14))))))