#lang typed/racket/base

(require "../types/types.rkt"
         "desugar.rkt"
         "auto-apply.rkt"
         "auto-cps.rkt"
         "parser.rkt")

(provide parse
         parser
         auto-cps
         auto-apply
         desugar)


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
