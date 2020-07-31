#lang typed/racket

(require "../types/types.rkt"
         "../ExpValues/values-sig.rkt"
         "../Environment/env-sig.rkt"
         "exp-sig.rkt")

(provide exp@)


(define-unit exp@
  (import values^ env^)
  (export exp^)

  (: symbol-exp [-> Symbol Exp])
  (define symbol-exp (λ (symbol) (make-symbol-exp symbol)))

  (: const-exp [-> Integer Exp])
  (define const-exp (λ (num) (make-const-exp num)))

  (: bool-exp [-> Boolean Exp])
  (define bool-exp (λ (bool) (make-bool-exp bool)))


  (: nullary-exp [-> Symbol Exp])
  (define nullary-exp (λ (op) (make-nullary-exp op)))

  (: unary-exp [-> Symbol Exp Exp])
  (define unary-exp (λ (op exp) (make-unary-exp op exp)))

  (: binary-exp [-> Symbol Exp Exp Exp])
  (define binary-exp (λ (op exp-1 exp-2) (make-binary-exp op exp-1 exp-2)))

  (: n-ary-exp [-> Symbol Exp * Exp])
  (define n-ary-exp (λ (op . exps) (make-n-ary-exp op exps)))


  (: if-exp [-> Exp Exp Exp Exp])
  (define if-exp
    (λ (pred-exp true-exp false-exp)
      (make-if-exp pred-exp true-exp false-exp)))

  (: cond-exp [-> (Listof (Pair Exp (Listof Exp))) Exp])
  (define cond-exp
    (λ (exps)
      (make-cond-exp (ann exps (Listof (Pair Exp (Listof Exp)))))))

  (: var-exp [-> Symbol Exp])
  (define var-exp (λ (var) (make-var-exp var)))

  (: let-exp [-> (Listof Symbol) (Listof Exp) Exp Exp])
  (define let-exp
    (λ (bound-vars bound-exps body)
      (make-let-exp bound-vars bound-exps body)))


  (: value-of [-> Exp Env ExpVal])
  (define value-of
    (λ (exp env)
      (cond [(symbol-exp? exp) (symbol-val (symbol-exp-symbol exp))]
            [(const-exp? exp) (num-val (const-exp-num exp))]
            [(bool-exp?  exp) (bool-val (bool-exp-bool exp))]

            [(if-exp? exp)
             (let ([pred-val (value-of (if-exp-pred-exp exp) env)])
               (if (expval->bool pred-val)
                   (value-of (if-exp-true-exp exp) env)
                   (value-of (if-exp-false-exp exp) env)))]
            [(cond-exp? exp)
             (let* ([exps (cond-exp-exps exp)]
                    [branch-exp
                     (assf (λ ([pred-exp : Exp])
                             (not (false? (value-of pred-exp env))))
                           exps)])
               (if (false? branch-exp)
                   (error 'value-of "cond-exp miss true banch!")
                   (value-of (cadr branch-exp) env)))]
            [(var-exp? exp) (cast (apply-env env (var-exp-var exp)) ExpVal)]
            [(let-exp? exp)
             (let ([vals (map (λ ([bound-exp : Exp]) : ExpVal
                                  (value-of bound-exp env))
                              (let-exp-bound-exps exp))])
               (value-of (let-exp-body exp)
                         (extend-env* (let-exp-bound-vars exp)
                                      vals
                                      env)))]

            [(nullary-exp? exp)
             ((hash-ref nullary-table (nullary-exp-op exp)))]
            [(unary-exp? exp)
             (let ([val : DenVal (cast (value-of (unary-exp-exp exp) env) DenVal)])
               ((hash-ref unary-table (unary-exp-op exp)) val))]
            [(binary-exp? exp)
             (let ([val-1 : DenVal (cast (value-of (binary-exp-exp-1 exp) env) DenVal)]
                   [val-2 : DenVal (cast (value-of (binary-exp-exp-2 exp) env) DenVal)])
               ((hash-ref binary-table (binary-exp-op exp)) val-1 val-2))]
            [(n-ary-exp? exp)
             (let ([vals : (Listof DenVal)
                         (map (λ ([exp : Exp]) : DenVal
                                  (cast (value-of exp env) DenVal))
                              (n-ary-exp-exps exp))])
               (hash-ref n-ary-table (n-ary-exp-op exp)) vals)]

            [else (raise-argument-error 'value-of "exp?" exp)])))



  (: nullary-table (Mutable-HashTable Symbol [-> ExpVal]))
  (define nullary-table (make-hasheqv))

  (: unary-table (Mutable-HashTable Symbol [-> DenVal ExpVal]))
  (define unary-table (make-hasheqv))

  (: binary-table (Mutable-HashTable Symbol [-> DenVal DenVal ExpVal]))
  (define binary-table (make-hasheqv))

  (: n-ary-table (Mutable-HashTable Symbol [-> (Listof DenVal) ExpVal]))
  (define n-ary-table (make-hasheqv))


  (: unary-arithmetic-pred [-> [-> Integer Boolean] [-> DenVal ExpVal]])
  (define unary-arithmetic-pred
    (λ (pred)
      (λ (val)
        (bool-val (pred (expval->num val))))))

  (: unary-arithmetic-func [-> [-> Integer Integer] [-> DenVal ExpVal]])
  (define unary-arithmetic-func
    (λ (func)
      (λ (val)
        (num-val (func (expval->num val))))))

  (: unary-IO-func [-> [-> Any Void] [-> DenVal ExpVal]])
  (define unary-IO-func
    (λ (func)
      (λ (val)
        (func (expval->s-expval val)))))


  (: binary-arithmetic-func [-> [-> Integer Integer Integer] [-> DenVal DenVal ExpVal]])
  (define binary-arithmetic-func
    (λ (func)
      (λ (val-1 val-2)
        (num-val (func (expval->num val-1) (expval->num val-2))))))

  (: binary-relation [-> [-> Integer Integer Boolean] [-> DenVal DenVal ExpVal]])
  (define binary-relation
    (λ (relation)
      (λ (val-1 val-2)
        (bool-val (relation (expval->num val-1) (expval->num val-2))))))


  (hash-set*! nullary-table
              ;; 'empty-list (nullary-list-func (λ () '()))
              'empty-list (λ () : ExpVal '())
              )

  (hash-set*! unary-table
              'zero? (unary-arithmetic-pred zero?)
              'minus (unary-arithmetic-func -)
              ;; 'car   (unary-pair-func car)
              ;; 'cdr   (unary-pair-func cdr)
              ;; 'null? (unary-list-pred null?)
              'car (λ ([val : DenVal]) : ExpVal (car (expval->pair val)))
              'cdr (λ ([val : DenVal]) : ExpVal (car (expval->pair val)))
              'null? (λ ([val : DenVal]) : ExpVal (bool-val (null? val)))


              'display (unary-IO-func display)
              'print (unary-IO-func print)
              'write (unary-IO-func write)

              'displayln (unary-IO-func displayln)
              'println (unary-IO-func println)
              'writeln (unary-IO-func writeln)
              )

  (hash-set*! binary-table
              '+ (binary-arithmetic-func +)
              '- (binary-arithmetic-func -)
              '* (binary-arithmetic-func *)
              '/ (binary-arithmetic-func quotient)
              'equal?   (binary-relation =)
              'greater? (binary-relation >)
              'less?    (binary-relation <)
              ;; 'cons    (binary-pair-func cons)
              'cons (λ ([val-1 : DenVal] [val-2 : DenVal]) : ExpVal
                        (pair-val (cons val-1 val-2)))
              )

  (hash-set*! n-ary-table
              ;; 'list (n-ary-list-func list)
              'list (λ ([vals : (Listof DenVal)]) : ExpVal (list-val vals))
              )

  )
