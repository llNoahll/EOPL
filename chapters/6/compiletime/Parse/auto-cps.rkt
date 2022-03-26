#lang typed/racket

(require "../types/types.rkt")

(provide auto-cps)


;; TODO
#;(define _ (gensym 'k-))
#;(define k? (curry symbol=? _))
#;(declare-refinement k?)
#;(define-type K (Refinement k?))
#;(define k (assert _ k?))

(define-type K Symbol)
(define k (gensym 'k-))

(define-type Simple-λ (List Lambda (List K)
                             (List (U Lambda Trace-Lambda) (U Symbol (Listof Symbol))
                                   CPS-Exp)))
(define-predicate simple-λ? Simple-λ)

(define-type Thread-Exp (U (List 'spawn       (List Simple-Exp K-Exp))
                           (List 'mutex       Simple-Exp)
                           (List 'wait        Simple-Exp)
                           (List 'signal      Simple-Exp)
                           (List 'kill-thread Simple-Exp)
                           (List 'thread-send Simple-Exp Simple-Exp)
                           (List 'thread-receive)
                           (List 'thread-try-receive)
                           (List 'yield)))

(define-type Simple-Exp (U Literal Symbol (List 'quote S-Exp) Simple-λ
                           (List 'set! Symbol Simple-Exp)
                           Thread-Exp))
(define-predicate simple-exp? Simple-Exp)

(define-type K-Exp (List Lambda (List Symbol) CPS-Exp))
(define-predicate k-exp? K-Exp)

(define-type Top-Exp (U (List K Simple-Exp)
                        (List* (List (U Symbol Simple-λ) (U K K-Exp))
                               (Listof Simple-Exp))))
(define-predicate top-exp? Top-Exp)

(define-type CPS-Exp (U Simple-Exp
                        Top-Exp
                        (List 'begin Simple-Exp CPS-Exp)
                        (List 'if Simple-Exp CPS-Exp CPS-Exp)
                        (List 'let (List (List K K-Exp)) CPS-Exp)
                        (List 'let (List (List Symbol (List 'λ (List '_) K))) CPS-Exp)))
(define-predicate cps-exp? CPS-Exp)


(: auto-cps [-> S-Exp CPS-Exp])
(define auto-cps
  (let ()
    (define-type CTX [-> Simple-Exp CPS-Exp])

    (: id CTX)
    (define id (λ (v) v))

    (: k0 K-Exp)
    (define k0 (let ([v (gensym 'v-)]) `(λ (,v) ,v)))

    (: ctx0 [-> Simple-Exp (List K Simple-Exp)])
    (define ctx0 (λ (v) `(,k ,v))) ; tail context

    (: fv (case-> [-> Symbol] [-> Natural Void]))
    (define fv
      (let ([n : Natural 0])
        (case-lambda
          [()
           (begin0 (gensym (string->symbol (string-append "v" (number->string n) "-")))
             (set! n (add1 n)))]
          [(m) (set! n m)])))


    (: cps [-> S-Exp CTX CPS-Exp])
    (define cps
      (λ (code ctx)
        (match code
          [(? simple-exp?) (ctx code)]

          [`(,(? λ?) ,args ,body-exp)
           #:when (and ((or/c symbol? (listof? symbol?)) args)
                       (s-exp? body-exp))
           (ctx `(λ (,k) (λ ,args ,(cps body-exp ctx0))))]
          [`(,(? trace-λ?) ,args ,body-exp)
           #:when (and ((or/c symbol? (listof? symbol?)) args)
                       (s-exp? body-exp))
           (ctx `(λ (,k) (trace-lambda ,args ,(cps body-exp ctx0))))]

          [`(set! ,var ,exp)
           #:when (and (symbol? var) (s-exp? exp))
           (cps exp (λ (val) (ctx `(set! ,var ,val))))]

          [`(begin ,exp ,exps ...)
           #:when (and (s-exp? exp)
                       ((listof? s-exp?) exps))
           (cps exp
                (λ (val)
                  (if (null? exps)
                      (ctx val)
                      `(begin ,val ,(cps `(begin ,@exps) ctx)))))]

          [`(if ,(? s-exp? pred-exp)
                ,(? s-exp? true-exp)
                ,(? s-exp? false-exp))
           (cps pred-exp
                (λ (p)
                  (if (or (eq? ctx ctx0) (eq? ctx id))
                      `(if ,p ,(cps true-exp ctx) ,(cps false-exp ctx))
                      (let ([v0 (fv)])
                        `(let ([,k (λ (,v0) ,(ctx v0))])
                           (if ,p ,(cps true-exp ctx0) ,(cps false-exp ctx0)))))))]

          [`(spawn       ,(? s-exp? exp)) (cps exp (λ (val) (ctx `(spawn       (,val ,k0)))))]
          [`(mutex       ,(? s-exp? exp)) (cps exp (λ (val) (ctx `(mutex       ,val))))]
          [`(wait        ,(? s-exp? exp)) (cps exp (λ (val) (ctx `(wait        ,val))))]
          [`(signal      ,(? s-exp? exp)) (cps exp (λ (val) (ctx `(signal      ,val))))]
          [`(kill-thread ,(? s-exp? exp)) (cps exp (λ (val) (ctx `(kill-thread ,val))))]

          [`(thread-send ,(? s-exp? tid-exp) ,(? s-exp? value-exp))
           (cps tid-exp
                (λ (tid)
                  (cps value-exp
                       (λ (val)
                         (ctx `(thread-send ,tid ,val))))))]
          ['(thread-receive)     (ctx code)]
          ['(thread-try-receive) (ctx code)]
          ['(yield)              (ctx code)]


          [`(let/cc ,cc-var ,body-exp)
           #:when (and (symbol? cc-var) (s-exp? body-exp))
           (if (or (eq? ctx ctx0) (eq? ctx id))
               `(let ([,cc-var (λ (_) ,k)])
                  ,(cps body-exp ctx))
               (let ([v0 (fv)])
                 `(let ([,k (λ (,v0) ,(ctx v0))])
                    (let ([,cc-var (λ (_) ,k)])
                      ,(cps body-exp ctx0)))))]

          [`(,exp ,exps ...)
           #:when (and (s-exp? exp) ((listof? s-exp?) exps))
           (cps exp
                (λ (op)
                  (with-asserts ([op (or/c symbol? simple-λ?)])
                    (let loop ([args : (Listof Simple-Exp) '()]
                               [exps exps])
                      (if (null? exps)
                          (if (eq? ctx ctx0)
                              `((,op ,k) ,@(reverse args))
                              (let ([vn (fv)])
                                `((,op (λ (,vn) ,(ctx vn))) ,@(reverse args))))
                          (cps (car exps)
                               (λ (arg)
                                 (loop (cons arg args) (cdr exps)))))))))])))

    (λ (code)
      ;; TODO: K type
      #;(cond [(cps-exp? code) code]
              [else
               (fv 0) (cps code id)])
      (fv 0) (cps code id))))
