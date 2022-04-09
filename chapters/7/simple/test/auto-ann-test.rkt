#lang typed/racket

(require "../Parse/parse.rkt")


(displayln "Start auto-ann test.\n")


(for ([code
       (in-list
        '(2
          -9
          x
          i
          #\a
          "b"
          '(1 2 3 4 5)
          (ann '(1 3 4 5 6) (Listof Natural))
          (set! noah "Noah Ma")
          (set! noah (ann "Noah Ma" String))
          (let ([noah : String ""]
                [dio  : Boolean #f])
            (set! noah "Noah Ma"))
          (begin
            (: noah String)
            (define noah "")
            (define dio #f)
            (set! noah "Noah Ma"))
          (begin
            (define dio #f)
            (: noah String)
            (define noah "")
            (set! noah "Noah Ma"))
          (begin
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
                   [else "Dummy" (displayln "Noah isn't named")]))))
          ))])
  (displayln "-----------------------------------")
  (pretty-print code)
  (pretty-print (auto-ann code))
  (pretty-print (desugar (auto-ann code)))
  (newline))
