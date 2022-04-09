#lang typed/racket/base

(require "../types/types.rkt"
         "auto-ann.rkt"
         "auto-apply.rkt"
         "auto-cps.rkt"
         "desugar.rkt"
         "parser.rkt")

(provide parse
         (all-from-out "desugar.rkt")
         (all-from-out "auto-ann.rkt")
         (all-from-out "auto-apply.rkt")
         (all-from-out "auto-cps.rkt")
         (all-from-out "desugar.rkt")
         (all-from-out "parser.rkt"))


(: parse [-> S-Exp S-Exp])
(define parse
  (λ (code)
    (parser
     (desugar
      (auto-cps
       (desugar
        (auto-apply
         (desugar
          (auto-ann
           code)))))))))
