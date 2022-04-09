#lang typed/racket

(require "../types/types.rkt"
         "../Parse/parse.rkt"
         "../Reference/ref-sig.rkt"
         "../Reference/ref-unit.rkt"
         "../Continuation/cont-sig.rkt"
         "../Continuation/cont-unit.rkt"
         "../ExpValues/values-sig.rkt"
         "../ExpValues/values-unit.rkt"
         "../Procedure/proc-sig.rkt"
         "../Procedure/proc-unit.rkt"
         "../TypeEnvironment/tenv-sig.rkt"
         "../TypeEnvironment/tenv-unit.rkt"
         "../Environment/env-sig.rkt"
         "../Environment/env-unit.rkt"
         "../Expressions/exp-sig.rkt"
         "../Expressions/exp-unit.rkt")

(require "../Modules/thread.rkt"
         "../Modules/exit.rkt")

(provide (all-from-out "../types/types.rkt")
         (all-from-out "../Parse/parse.rkt")
         (all-from-out "../Modules/thread.rkt")
         (all-from-out "../Modules/exit.rkt")
         (all-defined-out))


(define-compound-unit/infer base@
  (import)
  (export ref^ cont^ values^ env^ tenv^ proc^ exp^)
  (link   ref@ cont@ values@ env@ tenv@ proc@ exp@))

(define-values/invoke-unit base@
  (import)
  (export ref^ cont^ values^ env^ tenv^ proc^ exp^))


(define-namespace-anchor ns-anchor)
(define eval-ns (namespace-anchor->namespace ns-anchor))

(: *eval* [->* (S-Exp Env Cont) (Exact-Positive-Integer) ExpVal])
(define *eval*
  (λ (code env cont [timeslice 1])
    (: exp Exp)
    (define exp
      (assert (call-with-values
               (λ ()
                 (eval
                  (parser
                   (desugar
                    (auto-ann
                     code)))
                  eval-ns))
               (λ args (car args)))
              exp?))


    #;(pretty-print code)
    #;(pretty-print exp)
    (type-of exp (base-tenv) #f)
    (value-of/k exp env cont)))

(let ()
  (: +eval+ [-> S-Exp Env ExpVal])
  (define +eval+
    (λ (code env)
      (: exp Exp)
      (define exp
        (assert (call-with-values
                 (λ ()
                   (eval
                    (parser
                     (desugar
                      (auto-ann
                       code)))
                    eval-ns))
                 (λ args (car args)))
                exp?))

      #;(pretty-print code)
      #;(pretty-print exp)
      (type-of exp (base-tenv) #f)
      (value-of/k exp env (id-cont))))

  (: ~eval~ [-> S-Exp Env ExpVal])
  (define ~eval~
    (λ (code env)
      (: exp Exp)
      (define exp
        (assert (call-with-values
                 (λ ()
                   (eval
                    (parser
                     (desugar
                      (auto-ann
                       code)))
                    eval-ns))
                 (λ args (car args)))
                exp?))

      #;(pretty-print code)
      #;(pretty-print exp)
      (type-of exp (base-tenv) #f)
      (value-of/k exp env (id-cont))))

  (: nullary-func [-> Symbol [-> Any] [-> DenVal * ExpVal]])
  (define nullary-func
    (λ (name func)
      (λ vals
        (match vals
          ['() (s-expval->expval (func))]
          [_ (error name "Bad args: ~s" vals)]))))


  (: unary-pred [-> Symbol [-> Any Boolean] [-> DenVal * ExpVal]])
  (define unary-pred
    (λ (name pred)
      (λ vals
        (match vals
          [`(,val) (bool-val (pred (expval->denval val)))]
          [_ (error name "Bad args: ~s" vals)]))))

  (: unary-length (All (A) [-> Symbol (pred A) [-> A Index] [-> DenVal * ExpVal]]))
  (define unary-length
    (λ (name pred len)
      (λ vals
        (match vals
          [`(,val) #:when (pred val) (num-val (len val))]
          [_ (error name "Bad args: ~s" vals)]))))

  (: unary-destruct (All (A) [-> Symbol (pred A) [-> A DenVal] [-> DenVal * ExpVal]]))
  (define unary-destruct
    (λ (name pred destruct)
      (λ vals
        (match vals
          [`(,val) #:when (pred val) (destruct val)]
          [_ (error name "Bad args: ~s" vals)]))))

  (: binary-ref (All (A B) [-> Symbol (pred A) (pred B) [-> A B DenVal] [-> DenVal * ExpVal]]))
  (define binary-ref
    (λ (name pred key? ref)
      (λ vals
        (match vals
          [`(,val-1 ,val-2)
           #:when (and (pred val-1) (key? val-2))
           (ref val-1 val-2)]
          [_ (error name "Bad args: ~s" vals)]))))

  (: unary-arithmetic-pred [-> Symbol [-> Real Boolean] [-> DenVal * ExpVal]])
  (define unary-arithmetic-pred
    (λ (name pred)
      (λ vals
        (match vals
          [`(,val) (bool-val (pred (expval->num val)))]
          [_ (error name "Bad args: ~s" vals)]))))

  (: unary-arithmetic-func [-> Symbol [-> Real Real] [-> DenVal * ExpVal]])
  (define unary-arithmetic-func
    (λ (name func)
      (λ vals
        (match vals
          [`(,val) (num-val (func (expval->num val)))]
          [_ (error name "Bad args: ~s" vals)]))))

  (: unary-func [-> Symbol [-> Any Any] [-> DenVal * ExpVal]])
  (define unary-func
    (λ (name func)
      (λ vals
        (match vals
          [`(,val) (s-expval->expval (func val))]
          [_ (error name "Bad args: ~s" vals)]))))


  (: binary-imhash [-> Symbol
                       [->* () #:rest-star (DenVal DenVal) (Immutable-HashTable DenVal DenVal)]
                       [-> DenVal * ExpVal]])
  (define binary-imhash
    (λ (name make)
      (λ vals
        (match vals
          ['()              (make)]
          [`(,val-1 ,val-2) (make val-1 val-2)]
          [_ (error name "Bad args: ~s" vals)]))))

  (: unary-mhash [-> Symbol
                     [->* () ((Listof (Pairof DenVal DenVal))) (Mutable-HashTable DenVal DenVal)]
                     [-> DenVal * ExpVal]])
  (define unary-mhash
    (λ (name make)
      (λ vals
        (match vals
          ['()     (make)]
          [`(,val) #:when ((listof? denpair?) val) (make val)]
          [_ (error name "Bad args: ~s" vals)]))))

  (: binary-equal-relation [-> Symbol [-> Any Any Boolean] [-> DenVal * ExpVal]])
  (define binary-equal-relation
    (λ (name relation)
      (λ vals
        (match vals
          [`(,val-1 ,val-2)
           (bool-val (relation val-1 val-2))]
          [_ (error name "Bad args: ~s" vals)]))))


  (: binary-arithmetic-relation [-> Symbol [-> Real Real Boolean] [-> DenVal * ExpVal]])
  (define binary-arithmetic-relation
    (λ (name relation)
      (λ vals
        (match vals
          [`(,val-1 ,val-2)
           (bool-val (relation (expval->num val-1) (expval->num val-2)))]
          [_ (error name "Bad args: ~s" vals)]))))


  (: binary-arithmetic-func [-> Symbol [-> Real Real Real] [-> DenVal * ExpVal]])
  (define binary-arithmetic-func
    (λ (name func)
      (λ vals
        (match vals
          [`(,val-1 ,val-2)
           (num-val (func (expval->num val-1) (expval->num val-2)))]
          [_ (error name "Bad args: ~s" vals)]))))

  (: n-ary-arithmetic-func0 [-> Symbol [-> Real * Real] [-> DenVal * ExpVal]])
  (define n-ary-arithmetic-func0
    (λ (name func)
      (λ vals
        (num-val (apply func
                        (map (λ ([val : DenVal]) : Real
                               (expval->num val))
                             vals))))))

  (: n-ary-arithmetic-func1 [-> Symbol [-> Real Real * Real] [-> DenVal * ExpVal]])
  (define n-ary-arithmetic-func1
    (λ (name func)
      (λ vals
        (match vals
          [`(,val-1 . ,(? list? vals))
           (num-val (apply func
                           (expval->num val-1)
                           (map (λ ([val : DenVal]) : Real
                                  (expval->num val))
                                vals)))]
          [_ (error name "Bad args: ~s" vals)]))))

  (: n-ary-logic-func [-> [-> Boolean * Boolean] [-> DenVal * ExpVal]])
  (define n-ary-logic-func
    (λ (func)
      (λ vals
        (bool-val (apply func
                         (map (λ ([val : DenVal]) : Boolean
                                (expval->bool val))
                              vals))))))

  (: n-ary-func [-> Symbol [-> Any * Any] [-> DenVal * ExpVal]])
  (define n-ary-func
    (λ (name func)
      (λ vals
        (s-expval->expval (apply func vals)))))


  (: add-primitive-proc! [-> Symbol Type [-> DenVal * ExpVal] Void])
  (define add-primitive-proc!
    (λ (op-name op-type op-val)
      (add-denval! op-name op-type (primitive-proc op-val))))

  (: add-denval! [-> Symbol Type DenVal Void])
  (define add-denval!
    (λ (name type val)
      #;(displayln name)
      (let ([type (desugar-type type)])
        (if (has-tbinding? (base-tenv) name)
            (void (assert (=: type (assert (apply-tenv (base-tenv) name)))))
            (base-tenv (extend-tenv name type (base-tenv))))
        (if (has-binding?  (base-env)  name)
            (set-binding!  (base-env)  name val)
            (base-env  (extend-env  name val  (base-env)))))))


  ;; IO
  (add-primitive-proc! 'read    '[-> Any]  (nullary-func 'read    read))
  (add-primitive-proc! 'newline '[-> Void] (nullary-func 'newline newline))

  (add-primitive-proc! 'display '[-> Any Void] (unary-func 'display display))
  (add-primitive-proc! 'print   '[-> Any Void] (unary-func 'print   print))
  (add-primitive-proc! 'write   '[-> Any Void] (unary-func 'write   write))

  (add-primitive-proc! 'displayln '[-> Any Void] (unary-func 'displayln displayln))
  (add-primitive-proc! 'println   '[-> Any Void] (unary-func 'println   println))
  (add-primitive-proc! 'writeln   '[-> Any Void] (unary-func 'writeln   writeln))

  (add-primitive-proc! 'pretty-display '[-> Any Void] (unary-func 'pretty-display pretty-display))
  (add-primitive-proc! 'pretty-print   '[-> Any Void] (unary-func 'pretty-print   pretty-print))
  (add-primitive-proc! 'pretty-write   '[-> Any Void] (unary-func 'pretty-write   pretty-write))


  ;; Real
  (add-primitive-proc! '=  '[-> Real Real Boolean] (binary-arithmetic-relation '=  =))
  (add-primitive-proc! '>  '[-> Real Real Boolean] (binary-arithmetic-relation '>  >))
  (add-primitive-proc! '>= '[-> Real Real Boolean] (binary-arithmetic-relation '>= >=))
  (add-primitive-proc! '<  '[-> Real Real Boolean] (binary-arithmetic-relation '<  <))
  (add-primitive-proc! '<= '[-> Real Real Boolean] (binary-arithmetic-relation '<= <=))


  (add-primitive-proc! 'real? '[-> Any Boolean]  (unary-pred 'real? real?))
  (add-primitive-proc! 'zero? '[-> Real Boolean] (unary-arithmetic-pred 'zero? zero?))

  (add-primitive-proc! 'sub1  '[-> Real Real]  (unary-arithmetic-func 'sub1  sub1))
  (add-primitive-proc! 'add1  '[-> Real Real]  (unary-arithmetic-func 'add1  add1))
  (add-primitive-proc! '-1+   '[-> Real Real]  (unary-arithmetic-func '-1+   -1+))
  (add-primitive-proc! '1+    '[-> Real Real]  (unary-arithmetic-func '1+    1+))
  (add-primitive-proc! '1-    '[-> Real Real]  (unary-arithmetic-func '1-    1-))

  (add-primitive-proc! '+ '[-> Real * Real] (n-ary-arithmetic-func0 '+ +))
  (add-primitive-proc! '* '[-> Real * Real] (n-ary-arithmetic-func0 '* *))
  (add-primitive-proc! '- '[-> Real * Real] (n-ary-arithmetic-func1 '- -))
  (add-primitive-proc! '/ '[-> Real * Real] (n-ary-arithmetic-func1 '/ /))

  ;; Boolean
  (add-primitive-proc! 'boolean? '[-> Any Boolean] (unary-pred 'boolean? boolean?))
  (add-primitive-proc! 'not      '[-> Any Boolean] (unary-pred 'not      not))
  (add-primitive-proc! 'false?   '[-> Any Boolean] (unary-pred 'false?   false?))
  (add-primitive-proc! 'true?    '[-> Any Boolean] (unary-pred 'true?    true?))

  ;; Void
  (add-primitive-proc! 'void  '[-> Any * Void]  (n-ary-func 'void  void))
  (add-primitive-proc! 'void? '[-> Any Boolean] (unary-pred 'void? void?))

  ;; Undefined
  (add-denval! 'undefined 'Undefined undefined)
  (add-primitive-proc! 'undefined? '[-> Any Boolean] (unary-pred 'void? void?))

  )
