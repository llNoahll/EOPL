#lang typed/racket

(require "../../lambda-expression/typed-lambda-expression-version-2/lambda-expression-sig.rkt")

(provide parse-unparse^)


(define-signature parse-unparse^
  (
   [parse-expression : [-> Any Lc-Exp]]
   [unparse-lc-exp   : [-> Lc-Exp Any]]
   ))

