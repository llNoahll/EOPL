#lang typed/racket

(require "../types/types.rkt")

(provide (all-defined-out))


(: auto-ann [-> S-Exp S-Exp])
(define auto-ann
  (λ (code)
    (match code
      [`(ann ,exp ,type)
       #:when (and (s-exp? exp) (type? type))
       `(ann ,(auto-ann exp) ,type)]

      [`(quote ,(? symbol?))  code]
      [`(quote ,(? boolean?)) code]
      [`(quote ,(? real?))    code]
      [`(quote ,(? string?))  code]
      [`(quote ,(? char?))    code]
      [`(quote ,(? s-list? ls)) code]

      [(? boolean?) code]
      [(? real?)    code]
      [(? string?)  code]
      [(? char?)    code]

      [(? symbol?)  code]

      [`(set! ,var ,exp)
       #:when (and (symbol? var) (s-exp? exp))
       `(set! ,var ,(auto-ann exp))]

      [`(begin ,exps ..1)
       #:when ((listof? s-exp?) exps)
       (let loop ([vars  : (Listof Symbol) '()]
                  [types : (Listof (Option Type)) '()]
                  [vals  : (Listof S-Exp)  '()]
                  [exps  exps])
         (match exps
           [`((: ,var0 ,type0)
              (define ,var0 ,val0)
              ,exps0 ..1)
            #:when (and (type?   type0)
                        (symbol? var0)
                        (s-exp?  val0)
                        ((listof? s-exp?) exps0))
            (loop (cons var0  vars)
                  (cons type0 types)
                  (cons (auto-ann val0) vals)
                  exps0)]
           [`((define ,var0 ,val0)
              ,exps0 ..1)
            #:when (and (symbol? var0)
                        (s-exp?  val0)
                        ((listof? s-exp?) exps0))
            (loop (cons var0  vars)
                  (cons #f types)
                  (cons (auto-ann val0) vals)
                  exps0)]
           [_ (if (or (null? vars) (null? types) (null? vals))
                  `(begin ,@(map auto-ann exps))
                  (auto-ann
                   `(letrec ,(map (ann (λ (var type val)
                                         (if (false? type)
                                             `[,var ,val]
                                             `[,var : ,type ,val]))
                                       [-> Symbol (Option Type) S-Exp
                                           (U (List Symbol S-Exp)
                                              (List Symbol ': Type S-Exp))]
                                       #;(case-> [-> Symbol False S-Exp
                                                     (List Symbol S-Exp)]
                                                 [-> Symbol Type  S-Exp
                                                     (List Symbol ': Type S-Exp)]))
                                  (reverse vars)
                                  (reverse types)
                                  (reverse vals))
                      ,@exps)))]))]

      [`(if ,pred-exp ,true-exp ,false-exp)
       #:when (and (s-exp? pred-exp)
                   (s-exp? true-exp)
                   (s-exp? false-exp))
       `(if ,(auto-ann pred-exp)
            ,(auto-ann true-exp)
            ,(auto-ann false-exp))]
      [`(cond [,pred-exps ,body-exps ..1]
              ..1)
       #:when (and ((listof? s-exp?) pred-exps)
                   ((listof? (listof? s-exp?)) body-exps))
       `(cond ,@(map (ann (λ (pred-exp body-exps)
                            `[,(auto-ann pred-exp)
                              ,@(map auto-ann body-exps)])
                          [-> S-Exp S-List (Pair S-Exp S-List)])
                     pred-exps
                     (cast body-exps
                           (Pair (Pair S-Exp S-List)
                                 (Listof (Pair S-Exp S-List))))))]

      [`(and ,exps ...) #:when ((listof? s-exp?) exps) `(and ,@(map auto-ann exps))]
      [`(or  ,exps ...) #:when ((listof? s-exp?) exps) `(or  ,@(map auto-ann exps))]

      [`(with-handlers ([,pred-exps ,handler-exps] ...) ,body-exps ..1)
       #:when (and ((listof? s-exp?) pred-exps)
                   ((listof? s-exp?) handler-exps)
                   ((listof? s-exp?) body-exps))
       `(with-handlers ,(map (ann (λ (pred-exp handler-exp)
                                    `[,(auto-ann pred-exp)
                                      ,(auto-ann handler-exp)])
                                  [-> S-Exp S-Exp (List S-Exp S-Exp)])
                             pred-exps handler-exps)
          ,@(map auto-ann body-exps))]
      [`(mutex ,exp) #:when (s-exp? exp) `(mutex ,(auto-ann exp))]
      ['(mutex) code]
      [`(with-mutex ,exp ,body-exps ..1)
       #:when (and (s-exp? exp)
                   ((listof? s-exp?) body-exps))
       `(with-mutex ,(auto-ann exp)
          ,@(map auto-ann body-exps))]

      [`(,(? (λ (arg)
               (case arg
                 [(let let* letrec letrec*) #t]
                 [else #f]))
             let-op)
         ,binds
         ,body-exps
         ..1)
       #:when (and ((listof? s-exp?) binds)
                   ((listof? s-exp?) body-exps))
       (let loop ([bind-vars  : (Listof Symbol) '()]
                  [bind-types : (Listof (Option Type)) '()]
                  [bind-exps  : (Listof S-Exp)  '()]
                  [binds binds])
         (match binds
           [`([,bind-var : ,bind-type ,bind-exp]
              ,binds0 ...)
            #:when (and (type?   bind-type)
                        (symbol? bind-var)
                        (s-exp?  bind-exp))
            (loop (cons bind-var  bind-vars)
                  (cons bind-type bind-types)
                  (cons (auto-ann bind-exp)  bind-exps)
                  binds0)]
           [`([,bind-var ,bind-exp]
              ,binds0 ...)
            #:when (and (symbol? bind-var)
                        (s-exp?  bind-exp))
            (loop (cons bind-var  bind-vars)
                  (cons #f        bind-types)
                  (cons (auto-ann bind-exp)  bind-exps)
                  binds0)]
           ['()
            `(,let-op ,(map (ann (λ (var type exp)
                                   (if (false? type)
                                       `[,var ,exp]
                                       `[,var (ann ,exp ,type)]))
                                 [-> Symbol (Option Type) S-Exp
                                     (List Symbol (U S-Exp Ann-S-Exp))]
                                 #;(case-> [-> Symbol False S-Exp
                                               (List Symbol S-Exp)]
                                           [-> Symbol Type S-Exp
                                               (List Symbol Ann-S-Exp)]))
                            (reverse bind-vars)
                            (reverse bind-types)
                            (reverse bind-exps))
                      ,@(map auto-ann body-exps))]))]

      [`(let/cc ,cc-var ,body-exps ..1)
       #:when (and (symbol? cc-var) ((listof? s-exp?) body-exps))
       `(let/cc ,cc-var ,@(map auto-ann body-exps))]


      [`(,(and λ-op (or (? λ?) (? trace-λ?))) ,args ,body-exps ..1)
       #:when (and ((or/c symbol? (listof? symbol?)) args)
                   ((listof? s-exp?) body-exps))
       `(,λ-op ,args ,@(map auto-ann body-exps))]

      [`(,op ,exps ...)
       #:when (and (s-exp? op) ((listof? s-exp?) exps))
       `(,(auto-ann op) ,@(map auto-ann exps))]

      )))
