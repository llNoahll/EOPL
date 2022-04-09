#lang racket

(require (for-syntax racket/syntax)
         "../types/types.rkt")
(provide undefined)


(define cpsed-apply
  (λ (app)
    (λ (k)
      (λ (func args)
        (app (func k) args)))))

(define cpsed-func
  (λ (func)
    (λ (k)
      (λ args
        (k (apply func args))))))


(define _apply (cpsed-apply apply))
(provide (rename-out [_apply apply]))


(define-syntax (provide-auto-cpsed stx)
  (syntax-case stx ()
    [(_ func)
     (with-syntax ([_func (format-id #'func "_~a" #'func)])
       #'(begin
           (define _func (cpsed-func func))
           (provide (rename-out [_func func]))))]
    [(_ func funcs ...)
     #'(begin
         (provide-auto-cpsed func)
         (provide-auto-cpsed funcs ...))]))

(provide-auto-cpsed
 ;; get-nid
 ;; get-tid
 ;; get-ptid
 ;; mutex?

 eval
 identity
 empty-queue
 empty-list
 boolean?
 real?
 char?
 string?
 symbol?
 undefined?
 void?
 not
 false?
 true?
 null?
 list?
 empty-queue?
 queue?
 immutable?
 box?
 pair?
 vector?
 hash?
 exact-positive-integer?
 zero?
 sub1
 add1
 raise
 string-length
 length
 vector-length
 string-ref
 list-ref
 vector-ref
 hash-ref
 unbox
 car
 cdr
 caar
 cadr
 cdar
 cddr
 caaar
 caadr
 cadar
 caddr
 cdaar
 cdadr
 cddar
 cdddr
 caaaar
 caaadr
 caadar
 caaddr
 cadaar
 cadadr
 caddar
 cadddr
 cdaaar
 cdaadr
 cdadar
 cdaddr
 cddaar
 cddadr
 cdddar
 cddddr
 reverse
 read
 display
 print
 write
 displayln
 println
 writeln
 =
 >
 >=
 <
 <=
 cons
 enqueue
 box
 set-box!
 +
 *
 -
 /
 void
 list
 vector
 vector-immutable
 eq?
 eqv?
 equal?
 hash
 hasheq
 hasheqv
 make-hash
 make-hasheq
 make-hasheqv
 hash-has-key?
 hash-set
 hash-set!
 hash-remove
 hash-remove!
 hash-clear
 hash-clear!
 format
 call/cc
 )
