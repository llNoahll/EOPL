#lang typed/racket


(provide BinSTree contents-of lson rson)

(define-type BinSTree (U Null (List Integer BinSTree BinSTree)))

(: contents-of (case-> [-> Null Null]
                       [-> (List Integer BinSTree BinSTree) Integer]))
(define contents-of
  (λ (bstree)
    (if (null? bstree)
        '()
        (car bstree))))

(: lson [-> BinSTree BinSTree])
(define lson
  (λ (bstree)
    (if (null? bstree)
        (error 'lson "~s doesn't have son tree" bstree)
        (cadr bstree))))

(: rson [-> BinSTree BinSTree])
(define rson
  (λ (bstree)
    (if (null? bstree)
        (error 'lson "~s doesn't have son tree" bstree)
        (caddr bstree))))


(: path [-> Integer BinSTree (Listof (U 'left 'right))])
(define path
  (λ (num bstree)
    (cond [(null? bstree) '()]
          [(> num (contents-of bstree))
           (cons 'right (path num (rson bstree)))]
          [(< num (contents-of bstree))
           (cons 'left (path num (lson bstree)))]
          [else '()])))


;; (path 17 '(14 (7 () (12 () ()))
;;               (26 (20 (17 () ())
;;                       ())
;;                   (31 () ()))))
