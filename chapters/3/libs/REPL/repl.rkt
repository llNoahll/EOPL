#lang typed/racket

(require "../Parse/parser.rkt"
         "../types/version-1.rkt"
         "../ExpValues/typed-version-1/values-sig.rkt"
         "../ExpValues/typed-version-1/values-unit.rkt"
         "../Environment/typed-version-1/env-sig.rkt"
         "../Environment/typed-version-1/env-unit.rkt"
         "../Expressions/typed-version-1/exp-sig.rkt"
         "../Expressions/typed-version-1/exp-unit.rkt")

(provide *eval* *repl*)


(define-values/invoke-unit values@
  (import)
  (export values^))

(define-values/invoke-unit env@
  (import)
  (export env^))

(define-values/invoke-unit exp@
  (import values^ env^)
  (export exp^))


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
             (displayln (*eval* code env))
             (*repl* env)]))))
