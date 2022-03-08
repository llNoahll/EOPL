#lang typed/racket

(require "../Parse/auto-ann.rkt")


(pretty-print (auto-ann '2))
(pretty-print (auto-ann '-9))
(pretty-print (auto-ann 'x))
(pretty-print (auto-ann 'i))

(pretty-print (auto-ann '#\a))
(pretty-print (auto-ann '"b"))

(pretty-print (auto-ann ''(1 3 4 5 6)))
(pretty-print (auto-ann '(ann '(1 3 4 5 6) (Listof Natural))))

(pretty-print (auto-ann '(set! noah "Noah Ma")))
(pretty-print (auto-ann '(set! noah (ann "Noah Ma" String))))

(pretty-print (auto-ann '(begin
                           (define dio #f)

                           (: noah String)
                           (define noah "")

                           (set! noah "Noah Ma"))))

(pretty-print (auto-ann '(begin
                           (: noah (Option String))
                           (define noah #f)

                           (define dio #f)

                           (: jojo (Option Symbol))
                           (define jojo #f)

                           (set! noah "Noah Ma")

                           (if jojo
                               (set! jojo 'jojo)
                               ((ann (λ (arg) arg) [-> Any Any])
                                (cond
                                  [noah "Dummy" (displayln noah)]
                                  [else "Dummy" (displayln "Noah isn't named")]))))))
