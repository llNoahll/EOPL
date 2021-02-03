#lang typed/racket

(require "../types/types.rkt")

(provide (all-defined-out))


(: parser [-> S-Exp S-Exp])
(define parser
  (λ (code)
    (match code
      [`(quote ,(? symbol? symbol)) `(symbol-exp ',symbol)]
      [`(quote ,(? boolean? bool)) `(bool-exp ,bool)]
      [`(quote ,(? real? num)) `(const-exp ,num)]
      [`(quote ,(? string? str)) `(string-exp ,str)]
      [`(quote ,(? char? ch)) `(char-exp ,ch)]
      [`(quote ,(? s-list? ls))
       (parser `(list ,@(map (λ ([arg : S-Exp]) : S-Exp
                                 `(quote ,arg))
                             ls)))]

      [(? boolean? bool) `(bool-exp ,bool)]
      [(? real? num) `(const-exp ,num)]
      [(? string? str) `(string-exp ,str)]
      [(? char? ch) `(char-exp ,ch)]

      [(? symbol? var) `(var-exp ',var)]

      [`(begin
          (define ,(? symbol? #{vars : (Listof Any)})
            ,(? s-exp? #{vals : (Listof Any)})) ...
          ,(? s-exp? #{exps : S-List}) ...)
       (if (null? vars)
           `(begin-exp
              (list ,@(map parser exps)))
           (parser `(letrec ,(map (ann (λ (var val) (list var val))
                                       [-> Symbol S-Exp (List Symbol S-Exp)])
                                  (cast vars (Listof Symbol))
                                  (cast vals S-List))
                      ,@exps)))]

      [`(if ,(? s-exp? pred-exp)
            ,(? s-exp? true-exp)
            ,(? s-exp? false-exp))
       `(if-exp ,(parser pred-exp)
                ,(parser true-exp)
                ,(parser false-exp))]
      [`(cond [,(? s-exp? #{pred-exps : S-List})
               ,(? s-exp? #{body-exps : (Listof (Listof Any))})
               ...]
              ...)
       `(cond-exp
         (list ,@(map (λ ([pred-exp : S-Exp] [body-exps : S-List]) : S-Exp
                          `(list ,(parser (if (eq? pred-exp 'else)
                                              #t
                                              pred-exp))
                                 ,(parser `(begin ,@body-exps))))
                      pred-exps
                      (cast body-exps
                            (Listof (Listof S-Exp))))))]

      [`(and ,(? s-exp? #{exps : S-List}) ...)
       (if (null? exps)
           `(bool-exp #t)
           (parser `(if ,(car exps)
                        (and ,@(cdr exps))
                        #f)))]
      [`(or ,(? s-exp? #{exps : S-List}) ...)
       (if (null? exps)
           `(bool-exp #f)
           (parser `(if ,(car exps)
                        #t
                        (or ,@(cdr exps)))))]

      [`(let ([,(? symbol? #{bind-vars : (Listof Symbol)})
               ,(? s-exp?  #{bind-exps : S-List})]
              ...)
          ,(? s-exp? #{body-exps : S-List})
          ...)
       `(let-exp ',bind-vars
                 (list ,@(map parser bind-exps))
                 ,(parser `(begin ,@body-exps)))]
      [`(let* ([,(? symbol? #{bind-vars : (Listof Symbol)})
                ,(? s-exp?  #{bind-exps : S-List})]
               ...)
          ,(? s-exp? #{body-exps : S-List})
          ...)
       (parser
        (if (and (null? bind-vars) (null? bind-exps))
            `(let () ,@body-exps)
            `(let ([,(car bind-vars) ,(car bind-exps)])
               (let* (,@(map (λ ([var : Symbol] [exp : S-Exp]) : (List Symbol S-Exp)
                                 (list var exp))
                             (cdr bind-vars)
                             (cdr bind-exps)))
                 ,@body-exps))))]
      [`(letrec ([,(? symbol? #{bind-vars : (Listof Symbol)})
                  ,(? s-exp?  #{bind-exps : S-List})]
                 ...)
          ,(? s-exp? #{body-exps : S-List})
          ...)
       `(letrec-exp ',bind-vars
                    (list ,@(map parser bind-exps))
                    ,(parser `(begin ,@body-exps)))]
      [`(letrec* ([,(? symbol? #{bind-vars : (Listof Symbol)})
                   ,(? s-exp?  #{bind-exps : S-List})]
                  ...)
          ,(? s-exp? #{body-exps : S-List})
          ...)
       (parser
        (if (and (null? bind-vars) (null? bind-exps))
            `(letrec () ,@body-exps)
            `(letrec ([,(car bind-vars) ,(car bind-exps)])
               (letrec* (,@(map (λ ([var : Symbol] [exp : S-Exp]) : (List Symbol S-Exp)
                                    (list var exp))
                                (cdr bind-vars)
                                (cdr bind-exps)))
                 ,@body-exps))))]


      [`(,(? λ?) ,(? symbol? args)
                  ,(? s-exp? #{body-exps : S-List})
                  ...)
       `(proc-exp ',args ,(parser `(begin ,@body-exps)))]
      [`(,(? λ?) (,(? symbol? #{args : (Listof Symbol)}) ...)
                  ,(? s-exp? #{body-exps : S-List})
                  ...)
       `(proc-exp ',args ,(parser `(begin ,@body-exps)))]

      [`(,(? trace-λ?) ,(? symbol? args)
                        ,(? s-exp? #{body-exps : S-List})
                        ...)
       `(trace-proc-exp ',args ,(parser `(begin ,@body-exps)))]
      [`(,(? trace-λ?) (,(? symbol? #{args : (Listof Symbol)}) ...)
                        ,(? s-exp? #{body-exps : S-List})
                        ...)
       `(trace-proc-exp ',args ,(parser `(begin ,@body-exps)))]

      [`(,(? s-exp? op) ,(? s-exp? #{exps : S-List}) ...)
       `(call-exp ,(parser op)
                  (list ,@(map parser exps)))]

      )))
