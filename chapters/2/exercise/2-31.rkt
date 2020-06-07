#lang typed/racket


(define-type Prefix-Exp (U Const-Exp Diff-Exp))
(define-predicate prefix-exp? Prefix-Exp)

(define-struct const-exp ([num : Integer]) #:type-name Const-Exp)
(define-struct diff-exp ([operand1 : Prefix-Exp] [operand2 : Prefix-Exp]) #:type-name Diff-Exp)


(: make-prefix-exp [-> (Listof (U '- Integer)) Prefix-Exp])
(define make-prefix-exp
  (λ (ls)
    (: make-prefix-exp-tool [-> (Listof (U '- Integer))
                                (Values (Listof (U '- Integer))
                                        Prefix-Exp)])
    (define make-prefix-exp-tool
      (λ (ls)
        (match ls
          [(list (? integer? num))
           (values ls (const-exp num))]

          [(list '- (? integer? num-1) (? integer? num-2) _ ...)
           (values (cddr ls)
                   (diff-exp (const-exp num-1) (const-exp num-2)))]

          [(list '- (? integer? num) '- _ ...)
           (let-values ([([rson-end-position : (Listof (U '- Integer))]
                          [rson-exp : Prefix-Exp])
                         (make-prefix-exp-tool (cddr ls))])
             (values rson-end-position
                     (diff-exp (const-exp num) rson-exp)))]

          [(list '- '- _ ...)
           (let*-values ([([lson-end-position : (Listof (U '- Integer))]
                           [lson-exp : Prefix-Exp])
                          (make-prefix-exp-tool (cdr ls))]
                         [([rson-end-position : (Listof (U '- Integer))]
                           [rson-exp : Prefix-Exp])
                          (make-prefix-exp-tool (cdr lson-end-position))])
             (values rson-end-position
                     (diff-exp lson-exp rson-exp)))])))

    (define-values (end-position result)
      (make-prefix-exp-tool ls))

    result))


(: prefix-exp->list [-> Prefix-Exp (Listof (U '- Integer))])
(define prefix-exp->list
  (λ (exp)
    (cond [(const-exp? exp)
           (list (const-exp-num exp))]
          [(diff-exp? exp)
           (cons '-
                 (append (prefix-exp->list (diff-exp-operand1 exp))
                         (prefix-exp->list (diff-exp-operand2 exp))))])))


(displayln (prefix-exp->list (make-prefix-exp '(- - 3 2 - 4 - 12 7))))
(displayln (prefix-exp->list (make-prefix-exp '(- 1 2))))
(displayln (prefix-exp->list (make-prefix-exp '(0))))
