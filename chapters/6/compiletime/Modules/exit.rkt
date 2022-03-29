#lang typed/racket

(require "../types/types.rkt")

(provide module/exit)


(: module/exit [-> S-Exp S-Exp])
(define module/exit
  (λ (code)
    `(let/cc *exit*
       (set! exit *exit*)
       ,code)))
