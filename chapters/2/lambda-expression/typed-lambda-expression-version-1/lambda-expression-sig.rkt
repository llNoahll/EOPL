#lang typed/racket


(provide lambda-expression^
         Variable variable variable?
         Lambda lambda? (rename-out [lambda? λ?])
         Lc-Exp)

(define-signature lambda-expression^
  (
   ;; constructors
   [var-exp    : [-> Variable Lc-Exp]]
   [lambda-exp : [-> Variable Lc-Exp Lc-Exp]]
   [app-exp    : [-> Lc-Exp Lc-Exp Lc-Exp]]

   ;; predicates
   [var-exp?    : [-> Lc-Exp Boolean]]
   [lambda-exp? : [-> Lc-Exp Boolean]]
   [app-exp?    : [-> Lc-Exp Boolean]]
   [lc-exp?     : [-> Any Boolean]]

   ;; extractors
   [var-exp->var          : [-> Lc-Exp Variable]]
   [lambda-exp->bound-var : [-> Lc-Exp Variable]]
   [lambda-exp->body      : [-> Lc-Exp Lc-Exp]]
   [app-exp->rator        : [-> Lc-Exp Lc-Exp]]
   [app-exp->rand         : [-> Lc-Exp Lc-Exp]]
   ))


(define-new-subtype Variable (make-variable Symbol))
(define-type Lambda (U 'lambda 'λ))
(define-type Lc-Exp (U Variable
                       (List Lambda
                             (List Variable)
                             Lc-Exp)
                       (List Lc-Exp Lc-Exp)))


(: variable [-> Symbol Variable])
(define variable
  (λ (sym)
    (if (or (eqv? sym 'lambda)
            (eqv? sym 'λ))
        (raise-argument-error 'variable "variable?" sym)
        (make-variable sym))))

(: variable* [-> (U Symbol (Listof Symbol))
                 (U Variable (Listof Variable))])
(define variable*
  (λ (arg)
    (if (symbol? arg)
        (variable arg)
        (map (λ ([sym : Symbol]) : Variable
                 (variable sym))
             arg))))


(: variable? [-> Any Boolean :
                 #:+ (and Symbol (! Lambda))
                 #:- (or  (! Symbol) Lambda)
                 ])
(define variable?
  (λ (arg)
    (and (symbol? arg)
         (not (lambda? arg)))))


(: lc-exp? [-> Any Boolean])
(define lc-exp?
  (λ (arg)
    (match arg
      [(? lambda?) #f]
      [(? symbol?) #t]
      [(list (? lambda?)
             (list (? variable?))
             (? lc-exp?))
       #t]
      [(list (? lc-exp?) (? lc-exp?)) #t]
      [_ #f])))


(: lambda? [-> Any Boolean : Lambda])
(define-predicate lambda? Lambda)
