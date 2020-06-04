#lang typed/racket


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


;; (: path [-> Integer BinSTree (U (Listof (U 'left 'right))
;;                                 'false)])
;; (define path
;;   (λ (num bstree)
;;     (define path-iter
;;       (λ (path-stm ans)
;;         (if (stream-empty? path-stm)
;;             ans
;;             (path-iter
;;              (stream-filter
;;               (λ (stm) (not (null? (search-path stm))))
;;               (extend-path path-stm))
;;              (if (stream-ormap
;;                   (λ (stm) (= num (search-path stm)))
;;                   path-stm)
;;                  (stream-append
;;                   (stream-map (λ (stm) (= num (search-path stm)))
;;                               path-stm)
;;                   ans)
;;                  ans)))))

;;     (define search-path
;;       (λ (stm)
;;         ))

;;     (define extend-path
;;       (λ (path-stm)
;;         ))

;;     (reverse (stream->list
;;               (stream-map stream->list
;;                           (path-iter (stream empty-stream)
;;                                      empty-stream))))))


;; (: path [-> Integer BinSTree (U (Listof (U 'left 'right 'false))
;;                                 'false)])
;; (define path
;;   (λ (num bstree)
;;     (: path-iter [-> Integer BinSTree
;;                      (Listof (U 'left 'right 'false))
;;                      (Listof (U 'left 'right 'false))])
;;     (define path-iter
;;       (λ (num bstree way)
;;         (cond [(null? bstree)
;;                (cons 'false way)]
;;               [(= num (contents-of bstree))
;;                way]
;;               [else
;;                (let ([way-1 (path-iter num (lson bstree) (cons 'left way))])
;;                  (if (symbol=? (car way-1) 'false)
;;                      (path-iter num (rson bstree) (cons 'right way))
;;                      way-1))])))

;;     (let ([way (path-iter num bstree '())])
;;       (if (symbol=? (car way) 'false)
;;           'false
;;           (reverse way)))))


(path 17 '(14 (7 () (12 () ()))
              (26 (20 (17 () ())
                      ())
                  (31 () ()))))