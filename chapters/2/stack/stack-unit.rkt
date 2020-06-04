#lang typed/racket


(require/typed "stack-sig.rkt"
  [#:signature stack^
   ([empty-stack  : [-> Stack]]
    [empty-stack? : [-> Stack Boolean]]
    [push! : [-> Stack Any Void]]
    [pop!  : [-> Stack Void]]
    [top   : [-> Stack Any]])])

(provide stack@ Stack)


(define-type Stack (Parameter (Listof Any)))


(define-unit stack@
  (import)
  (export stack^)


  (: empty-stack [-> Stack])
  (define (empty-stack)
    (make-parameter '()))

  (: empty-stack? [-> Stack Boolean])
  (define empty-stack?
    (λ (arg)
      (null? (arg))))


  (: push! [-> Stack Any Void])
  (define push!
    (λ (stack val)
      (stack (cons val (stack)))))

  (: pop! [-> Stack Void])
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
          (car (stack))))))
