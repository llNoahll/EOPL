#lang typed/racket

(require "../stack/stack-unit.rkt")

(require/typed "../stack/stack-sig.rkt"
  [#:signature stack^
   ([empty-stack  : [-> Stack]]
    [empty-stack? : [-> Stack Boolean]]
    [push! : [-> Stack Any Void]]
    [pop!  : [-> Stack Void]]
    [top   : [-> Stack Any]])])


(define-values/invoke-unit stack@
  (import)
  (export stack^))


(define stack-1 (empty-stack))
(define stack-2 (empty-stack))

(push! stack-1 1)
(push! stack-1 2)
(push! stack-1 3)
(push! stack-1 4)
(displayln (top stack-1))
(displayln (stack-1))
(pop! stack-1)
(pop! stack-1)
(displayln (top stack-1))
(displayln (stack-1))

(push! stack-2 1)
(push! stack-2 2)
(push! stack-2 3)
(push! stack-2 4)
(displayln (top stack-2))
(displayln (stack-2))
(pop! stack-2)
(pop! stack-2)
(displayln (top stack-2))
(displayln (stack-2))
