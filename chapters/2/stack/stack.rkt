#lang racket

  (define (empty-stack)
    (make-parameter '()
                    (lambda (val)
                      val)))

  (define-predicate empty-stack? Empty-Stack)


  (define push!
    (λ (stack val)
      (stack (cons val (stack)))))

  (define pop!
    (λ (stack)
      (if (empty-stack? stack)
          (error 'pop! "Empty Stack!")
          (stack (cdr (stack))))))

  (: top [-> Stack Any])
  (define top
    (λ (stack)
      (if (empty-stack? stack)
          (error 'top "Empty Stack!")
          (cdr (stack)))))


;; (define stack-2 (empty-stack))
;; (push! stack-2 1)
;; (push! stack-2 2)
;; (push! stack-2 3)
;; (push! stack-2 4)
;; (displayln (top stack-2))
;; (displayln stack-2)
;; (pop! stack-2)
;; (pop! stack-2)
;; (displayln (top stack-2))
;; (displayln stack-2)


;; (define stack-1 (empty-stack))
;; (push! stack-1 1)
;; (push! stack-1 2)
;; (push! stack-1 3)
;; (push! stack-1 4)
;; (displayln (top stack-1))
;; (displayln stack-1)
;; (pop! stack-1)
;; (pop! stack-1)
;; (displayln (top stack-1))
;; (displayln stack-1)
