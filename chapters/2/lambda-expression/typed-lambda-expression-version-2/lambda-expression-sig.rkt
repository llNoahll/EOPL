#lang typed/racket


(provide lambda-expression^
         Variable variable variable?

         Lambda lambda? (rename-out [lambda? λ?])
         Var-Exp Lambda-Exp App-Exp Lc-Exp

         struct-var-exp struct-lambda-exp struct-app-exp
         struct-var-exp-var
         struct-lambda-exp-var struct-lambda-exp-body
         struct-app-exp-rator struct-app-exp-rand)


(define-signature lambda-expression^
  (
   ;; constructors
   [var-exp    : [-> Variable Var-Exp]]
   [lambda-exp : [-> Var-Exp Lc-Exp Lambda-Exp]]
   [app-exp    : [-> Lc-Exp Lc-Exp App-Exp]]

   ;; predicates
   [var-exp?    : [-> Any Boolean : Var-Exp]]
   [lambda-exp? : [-> Any Boolean : Lambda-Exp]]
   [app-exp?    : [-> Any Boolean : App-Exp]]
   [lc-exp?     : [-> Any Boolean : Lc-Exp]]

   ;; extractors
   [var-exp->var          : [-> Var-Exp Variable]]
   [lambda-exp->bound-var : [-> Lambda-Exp Var-Exp]]
   [lambda-exp->body      : [-> Lambda-Exp Lc-Exp]]
   [app-exp->rator        : [-> App-Exp Lc-Exp]]
   [app-exp->rand         : [-> App-Exp Lc-Exp]]
   ))


(define-new-subtype Variable (make-variable Symbol))
(define-type Lambda (U 'lambda 'λ))
(define-type Lc-Exp (U Var-Exp Lambda-Exp App-Exp))


(: variable [-> Symbol Variable])
(define variable
  (λ (sym)
    (if (or (eqv? sym 'lambda)
            (eqv? sym 'λ))
        (raise-argument-error 'variable "variable?" sym)
        (make-variable sym))))

(: variable? [-> Any Boolean])
(define variable?
  (λ (arg)
    (and (symbol? arg)
         (not (lambda? arg)))))


(: lambda? [-> Any Boolean : Lambda])
(define-predicate lambda? Lambda)


(define-struct struct-var-exp ([var : Variable]) #:type-name Var-Exp)
(define-struct struct-lambda-exp ([var : Var-Exp] [body : Lc-Exp]) #:type-name Lambda-Exp)
(define-struct struct-app-exp ([rator : Lc-Exp] [rand : Lc-Exp]) #:type-name App-Exp)
