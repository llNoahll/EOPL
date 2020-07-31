#lang typed/racket

(require "../base/base.rkt"
         "../Parse/parser.rkt")

(provide *eval* *repl*)


(define-namespace-anchor ns-anchor)
(define eval-ns (namespace-anchor->namespace ns-anchor))


(: *eval* [-> S-Exp Env ExpVal])
(define *eval*
  (λ (code env)
    (: exp Exp)
    (define exp
      (cast (call-with-values
             (λ () (eval (parser code) eval-ns))
             (λ args (car args)))
            Exp))

    ;; (pretty-print code)
    (value-of exp env)))

(: *repl* [-> Env Void])
(define *repl*
  (λ (env)
    (display "]=> ")
    (let ([code : S-Exp (cast (read) S-Exp)])
      (cond [(equal? code '(exit))
             (void)]
            [else
             (writeln (*eval* code env))
             (*repl* env)]))))
