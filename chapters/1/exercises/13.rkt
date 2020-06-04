#! /usr/bin/env racket
#lang typed/racket


;; ;; for quasiquote.
;; (define-type Constant (U Number Char String Boolean Null Void))
;; (define-type S-List (Pairof S-Expr S-Expr))
;; (define-type S-Expr (U Symbol Constant S-List))

;; for quote.
(define-type Constant (U Number Char String Boolean Null))
(define-type S-List (Listof S-Expr))
(define-type S-Expr (U Symbol Constant S-List))


(: subst [-> Symbol Symbol S-List S-List])
(define subst
  (λ (s-new s-old s-list)
    (: subst-in-s-expr [-> Symbol Symbol S-Expr S-Expr])
    (define subst-in-s-expr
      (λ (s-new s-old s-expr)
        (if (symbol? s-expr)
            (if (eqv? s-old s-expr) s-new s-expr)
            (subst s-new s-old s-expr))))

    (map (λ ([s-expr : S-Expr])
           (subst-in-s-expr s-new s-old s-expr))
         s-list)))


(displayln (subst 'a 'b '((b c) (b () d))))
(displayln (subst 'a 'b '((b c) (b () d) b)))
(displayln (subst 'a 'b '((b c) (b () d) a)))
(displayln (subst 'a 'b '((b c) (b () d) c)))