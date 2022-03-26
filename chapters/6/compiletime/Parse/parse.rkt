#lang typed/racket/base

(require "../types/types.rkt"
         "desugar.rkt"
         "auto-apply.rkt"
         "auto-cps.rkt"
         "parser.rkt")

(provide parse)


(: parse [-> S-Exp S-Exp])
(define parse
  (λ (code)
    (parser
     (desugar
      (auto-cps
       (desugar
        (auto-apply
         (desugar
          code))))))))
