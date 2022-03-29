#lang typed/racket

(require "../base/base.rkt")


(for ([code
       (in-list
        '(2))])
  (displayln "---------------------------")
  (pretty-print
   (module/thread
    (auto-apply
     (desugar
      code))
    1))
  (newline)

  (displayln "---------------------------")
  (pretty-print
   (desugar
    (module/thread
     (auto-apply
      (desugar
       code))
     1)))
  (newline)

  (displayln "---------------------------")
  (pretty-print
   (desugar
    (auto-cps
     (desugar
      (module/thread
       (auto-apply
        (desugar
         code))
       1)))))
  (newline))
