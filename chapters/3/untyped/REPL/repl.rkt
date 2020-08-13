#lang racket

(require "../base/base.rkt"
         "../Parse/parser.rkt")

(provide *repl*)


(define *repl*
  (λ (env)
    (display "]=> ")
    (let ([code (read)])
      (case code
        ['(exit) (void)]
        [else
         (writeln (*eval* code env))
         (*repl* env)]))))


;; (*repl* (base-env))
