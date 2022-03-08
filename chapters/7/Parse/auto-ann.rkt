#lang typed/racket

(require "../types/types.rkt")

(provide (all-defined-out))


(: auto-ann [-> S-Exp S-Exp])
(define auto-ann
  (λ (code)
    (match code
      [`(ann ,(? s-exp? exp) ,(? type? type)) `(ann ,(auto-ann exp) ,type)]

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

      [`(set! ,(? symbol? var) ,(? s-exp? exp)) `(set! ,var ,(auto-ann exp))]

      [`(begin ,(? s-exp? #{exps : S-List}) ..1)
       (let loop ([vars  : (Listof Symbol) '()]
                  [types : (Listof (Option Type)) '()]
                  [vals  : (Listof S-Exp)  '()]
                  [exps  exps])
         (match exps
           [`((: ,(? symbol? var0) ,(? type? type0))
              (define ,var0 ,(? s-exp? val0))
              ,(? s-exp? #{exps0 : S-List}) ..1)
            (loop (cons var0  vars)
                  (cons type0 types)
                  (cons (auto-ann val0) vals)
                  exps0)]
           [`((define ,(? symbol? var0) ,(? s-exp? val0))
              ,(? s-exp? #{exps0 : S-List}) ..1)
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
                                       (case-> [-> Symbol False S-Exp
                                                   (List Symbol S-Exp)]
                                               [-> Symbol Type  S-Exp
                                                   (List Symbol ': Type S-Exp)]
                                               [-> Symbol (Option Type) S-Exp
                                                   (U (List Symbol S-Exp)
                                                      (List Symbol ': Type S-Exp))]))
                                  (reverse vars)
                                  (reverse types)
                                  (reverse vals))
                      ,@exps)))]))]

      [`(if ,(? s-exp? pred-exp)
            ,(? s-exp? true-exp)
            ,(? s-exp? false-exp))
       `(if ,(auto-ann pred-exp)
            ,(auto-ann true-exp)
            ,(auto-ann false-exp))]
      [`(cond [,(? s-exp? #{pred-exps : S-List})
               ,(? s-exp? #{body-exps : (Listof (Listof Any))})
               ..1]
              ..1)
       `(cond ,@(map (ann (λ (pred-exp body-exps)
                            `[,(auto-ann pred-exp)
                              ,@(map auto-ann body-exps)])
                          [-> S-Exp S-List (Pair S-Exp S-List)])
                     pred-exps
                     (cast body-exps
                           (Pair (Pair S-Exp S-List)
                                 (Listof (Pair S-Exp S-List))))))]

      [`(and ,(? s-exp? #{exps : S-List}) ...) `(and ,@(map auto-ann exps))]
      [`(or  ,(? s-exp? #{exps : S-List}) ...) `(or  ,@(map auto-ann exps))]

      [`(with-handlers ([,(? s-exp? #{pred-exps : S-List})
                         ,(? s-exp? #{handler-exps : S-List})]
                        ...)
          ,(? s-exp? #{body-exps : S-List})
          ..1)
       `(with-handlers ,(map (ann (λ (pred-exp handler-exp)
                                    `[,(auto-ann pred-exp)
                                      ,(auto-ann handler-exp)])
                                  [-> S-Exp S-Exp (List S-Exp S-Exp)])
                             pred-exps handler-exps)
          ,@(map auto-ann body-exps))]
      [`(raise       ,(? s-exp? exp)) `(raise       ,(auto-ann exp))]
      [`(spawn       ,(? s-exp? exp)) `(spawn       ,(auto-ann exp))]
      [`(mutex       ,(? s-exp? exp)) `(mutex       ,(auto-ann exp))]
      [`(wait        ,(? s-exp? exp)) `(wait        ,(auto-ann exp))]
      [`(signal      ,(? s-exp? exp)) `(signal      ,(auto-ann exp))]
      [`(kill-thread ,(? s-exp? exp)) `(kill-thread ,(auto-ann exp))]

      [`(thread-send ,(? s-exp? tid-exp) ,(? s-exp? value-exp))
       `(thread-send ,(auto-ann tid-exp) ,(auto-ann value-exp))]
      ['(thread-receive)     code]
      ['(thread-try-receive) code]
      ['(yield)              code]
      ['(mutex)              code]
      [`(with-mutex ,(? s-exp? exp)
          ,(? s-exp? #{body-exps : S-List})
          ..1)
       `(with-mutex ,(auto-ann exp)
          ,@(map auto-ann body-exps))]

      [`(,(? (λ (arg)
               (case arg
                 [(let let* letrec letrec*) #t]
                 [else #f]))
             let-op)
         ,(? s-list? binds)
         ,(? s-exp? #{body-exps : S-List})
         ..1)

       (let loop ([bind-vars  : (Listof Symbol) '()]
                  [bind-types : (Listof (Option Type)) '()]
                  [bind-exps  : (Listof S-Exp)  '()]
                  [binds binds])
         (match binds
           [`([,(? symbol? bind-var)
               : ,(? type? bind-type)
               ,(? s-exp?  bind-exp)]
              ,binds0 ...)
            (loop (cons bind-var  bind-vars)
                  (cons bind-type bind-types)
                  (cons (auto-ann bind-exp)  bind-exps)
                  binds0)]
           [`([,(? symbol? bind-var)
               ,(? s-exp?  bind-exp)]
              ,binds0 ...)
            (loop (cons bind-var  bind-vars)
                  (cons #f        bind-types)
                  (cons (auto-ann bind-exp)  bind-exps)
                  binds0)]
           ['()
            `(,let-op ,(map (ann (λ (var type exp)
                                   (if (false? type)
                                       `[,var ,exp]
                                       `[,var (ann ,exp ,type)]))
                                 (case-> [-> Symbol False S-Exp
                                             (List Symbol S-Exp)]
                                         [-> Symbol Type S-Exp
                                             (List Symbol (List 'ann S-Exp Type))]
                                         [-> Symbol (Option Type) S-Exp
                                             (U (List Symbol S-Exp)
                                                (List Symbol (List 'ann S-Exp Type)))]))
                            (reverse bind-vars)
                            (reverse bind-types)
                            (reverse bind-exps))
                      ,@(map auto-ann body-exps))]))]

      [`(let/cc ,(? symbol? cc-var)
            ,(? s-exp? #{body-exps : S-List})
            ..1)
       `(let/cc ,cc-var ,@(map auto-ann body-exps))]


      [`(,(and λ-op (or (? λ?) (? trace-λ?)))
         ,(? (ann (λ (arg)
                    (or (symbol? arg)
                        ((listof? symbol?) arg)))
                  [-> Any Boolean : (U Symbol (Listof Symbol))])
             args)
         ,(? s-exp? #{body-exps : S-List})
         ..1)
       `(,λ-op ,args ,@(map auto-ann body-exps))]

      [`(,(? s-exp? op) ,(? s-exp? #{exps : S-List}) ...)
       `(,(auto-ann op) ,@(map auto-ann exps))]

      )))
