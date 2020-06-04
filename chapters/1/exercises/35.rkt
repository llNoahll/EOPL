#lang typed/racket

(require "31.rkt")


;; (: number-leaves [-> BinTree BinTree])
;; (define number-leaves
;;   (λ (btree)
;;     (: leaf-number Integer)
;;     (define leaf-number -1)

;;     (: number-leaves-iter [-> BinTree BinTree])
;;     (define number-leaves-iter
;;       (λ (btree)
;;         (cond [(leaf? btree)
;;                (set! leaf-number (add1 leaf-number))
;;                (leaf leaf-number)]
;;               [else (interior-node (contents-of btree)
;;                                    (number-leaves-iter (lson btree))
;;                                    (number-leaves-iter (rson btree)))])))


;;     (number-leaves-iter btree)))


(: number-leaves [-> BinTree BinTree])
(define number-leaves
  (λ (btree)
    (: number-leaves-iter [-> BinTree Natural (List Natural BinTree)])
    (define number-leaves-iter          ; (next-num current-btree)
      (λ (btree num)
        (if (leaf? btree)
            (list (add1 num) num)
            (let* ([ltree (number-leaves-iter (lson btree) num)]
                   [rtree (number-leaves-iter (rson btree) (car ltree))])
              (list (car rtree)
                    (interior-node (contents-of btree)
                                   (cadr ltree)
                                   (cadr rtree)))))))


    (cadr (number-leaves-iter btree 0))))



(number-leaves
 (interior-node 'foo
                (interior-node 'bar
                               (leaf 26)
                               (leaf 12))
                (interior-node 'baz
                               (leaf 11)
                               (interior-node 'quux
                                              (leaf 117)
                                              (leaf 14)))))