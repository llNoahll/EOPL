#lang racket

(provide (all-defined-out))


(define true? (λ (arg) (not (false? arg))))
(define string-empty? (λ (arg) (eq? arg "")))


(define s-exp?
  (λ (arg)
    (or (boolean? arg)
        (real? arg)
        (symbol? arg)
        (char? arg)
        (string? arg)
        (s-list? arg))))

(define s-list?
  (λ (arg)
    (and (list? arg)
         (andmap s-exp? arg))))


(define λ? (λ (arg) (or (eq? arg 'λ) (eq? arg 'lambda))))
(define lambda? (λ (arg) (or (eq? arg 'λ) (eq? arg 'lambda))))


(define-struct env
  (type
   has-binding?
   apply-env)
  #:transparent)


(define-struct proc
  (vars
   body
   saved-env)
  #:transparent)


;; (define-type Exp (U Symbol-Exp Const-Exp Bool-Exp Char-Exp String-Exp
;;                     If-Exp Cond-Exp
;;                     Begin-Exp Primitive-Proc-Exp Proc-Exp Call-Exp
;;                     Var-Exp Let-Exp))
(define exp?
  (λ (arg)
    (or (symbol-exp? arg)
        (const-exp? arg)
        (bool-exp? arg)
        (char-exp? arg)
        (string-exp? arg)

        (if-exp? arg)
        (cond-exp? arg)

        (begin-exp? arg)
        (primitive-proc-exp? arg)
        (proc-exp? arg)
        (call-exp? arg)

        (var-exp? arg)
        (let-exp? arg)
        )))


(define-struct symbol-exp (symbol)
  #:transparent)

(define-struct const-exp (num)
  #:transparent)

(define-struct bool-exp (bool)
  #:transparent)

(define-struct char-exp (char)
  #:transparent)

(define-struct string-exp (str)
  #:transparent)


(define-struct if-exp
  (pred-exp
   true-exp
   false-exp)
  #:transparent)

(define-struct cond-exp (exps)
  #:transparent)


(define-struct var-exp (var)
  #:transparent)

(define-struct let-exp
  (bound-vars
   bound-exps
   body)
  #:transparent)


(define-struct begin-exp (exps)
  #:transparent)


(define-struct primitive-proc-exp (op exps)
  #:transparent)

(define-struct proc-exp (vars body)
  #:transparent)

(define-struct call-exp (rator rands)
  #:transparent)
