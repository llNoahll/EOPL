#lang typed/racket

(require "../types/types.rkt")

(provide (all-defined-out))


(: parser [-> S-Exp S-Exp])
(define parser
  (λ (code)
    (match code
      [`(quote ,(? symbol? symbol)) `(symbol-exp ',symbol)]
      [`(quote ,(? boolean? bool))  `(bool-exp ,bool)]
      [`(quote ,(? real? num))      `(const-exp ,num)]
      [`(quote ,(? string? str))    `(string-exp ,str)]
      [`(quote ,(? char? ch))       `(char-exp ,ch)]
      [`(quote ,(? s-list? ls))
       (parser `(list ,@(map (ann (λ (arg) `(quote ,arg)) [-> S-Exp S-Exp]) ls)))]

      [(? boolean? bool) `(bool-exp ,bool)]
      [(? real? num)     `(const-exp ,num)]
      [(? string? str)   `(string-exp ,str)]
      [(? char? ch)      `(char-exp ,ch)]

      [(? symbol? var) `(var-exp ',var)]

      [`(set! ,(? symbol? var) ,(? s-exp? exp))
       `(assign-exp ',var ,(parser exp))]

      [`(begin
          (define ,(? symbol? #{vars : (Listof Any)})
            ,(? s-exp? #{vals : (Listof Any)})) ...
          ,(? s-exp? #{exps : S-List}) ..1)
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
               ..1]
              ..1)
       `(cond-exp
         (list ,@(map (ann (λ (pred-exp body-exps)
                             `(list ,(parser (if (eq? pred-exp 'else)
                                                 #t
                                                 pred-exp))
                                    ,(parser `(begin ,@body-exps))))
                           [-> S-Exp S-List S-Exp])
                      pred-exps
                      (cast body-exps
                            (Pair (Pair S-Exp S-List)
                                  (Listof (Pair S-Exp S-List)))))))]

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

      [`(with-handlers ([,(? s-exp? #{pred-exps : S-List})
                         ,(? s-exp? #{handler-exps : S-List})]
                        ...)
          ,(? s-exp? #{body-exps : S-List})
          ..1)
       `(handlers-exp (list ,@(map parser pred-exps))
                      (list ,@(map parser handler-exps))
                      ,(parser `(begin ,@body-exps)))]
      [`(raise       ,(? s-exp? exp)) `(raise-exp  ,(parser exp))]
      [`(spawn       ,(? s-exp? exp)) `(spawn-exp  ,(parser exp))]
      [`(mutex       ,(? s-exp? exp)) `(mutex-exp  ,(parser exp))]
      [`(wait        ,(? s-exp? exp)) `(wait-exp   ,(parser exp))]
      [`(signal      ,(? s-exp? exp)) `(signal-exp ,(parser exp))]
      [`(kill-thread ,(? s-exp? exp)) `(kill-exp   ,(parser exp))]

      [`(thread-send ,(? s-exp? tid-exp) ,(? s-exp? value-exp))
       `(send-exp ,(parser tid-exp) ,(parser value-exp))]
      ['(thread-receive)     '(receive-exp)]
      ['(thread-try-receive) '(try-receive-exp)]
      ['(yield)              '(yield-exp)]

      ['(mutex) (parser '(mutex 1))]
      [`(with-mutex ,(? s-exp? exp)
          ,(? s-exp? #{body-exps : S-List})
          ..1)
       (define mut (gensym 'mut))
       (parser
        `(let ([,mut ,exp])
           (wait ,mut)
           ,@body-exps
           (signal ,mut)))]

      [`(let ([,(? symbol? #{bind-vars : (Listof Symbol)})
               ,(? s-exp?  #{bind-exps : S-List})]
              ...)
          ,(? s-exp? #{body-exps : S-List})
          ..1)
       `(let-exp ',bind-vars
                 (list ,@(map parser bind-exps))
                 ,(parser `(begin ,@body-exps)))]
      [`(let* ([,(? symbol? #{bind-vars : (Listof Symbol)})
                ,(? s-exp?  #{bind-exps : S-List})]
               ...)
          ,(? s-exp? #{body-exps : S-List})
          ..1)
       (parser
        (if (and (null? bind-vars) (null? bind-exps))
            `(let () ,@body-exps)
            `(let ([,(car bind-vars) ,(car bind-exps)])
               (let* (,@(map (ann (λ (var exp) (list var exp))
                                  [-> Symbol S-Exp (List Symbol S-Exp)])
                             (cdr bind-vars)
                             (cdr bind-exps)))
                 ,@body-exps))))]
      [`(letrec ([,(? symbol? #{bind-vars : (Listof Symbol)})
                  ,(? s-exp?  #{bind-exps : S-List})]
                 ...)
          ,(? s-exp? #{body-exps : S-List})
          ..1)
       `(letrec-exp ',bind-vars
                    (list ,@(map parser bind-exps))
                    ,(parser `(begin ,@body-exps)))]
      [`(letrec* ([,(? symbol? #{bind-vars : (Listof Symbol)})
                   ,(? s-exp?  #{bind-exps : S-List})]
                  ...)
          ,(? s-exp? #{body-exps : S-List})
          ..1)
       (parser
        (if (and (null? bind-vars) (null? bind-exps))
            `(letrec () ,@body-exps)
            `(letrec ([,(car bind-vars) ,(car bind-exps)])
               (letrec* (,@(map (ann (λ (var exp) (list var exp))
                                     [-> Symbol S-Exp (List Symbol S-Exp)])
                                (cdr bind-vars)
                                (cdr bind-exps)))
                 ,@body-exps))))]


      [`(let/cc ,(? symbol? cc-var)
          ,(? s-exp? #{body-exps : S-List})
          ..1)
       `(let/cc-exp ',cc-var ,(parser `(begin ,@body-exps)))]


      [`(,(? λ?) ,(? symbol? args)
                  ,(? s-exp? #{body-exps : S-List})
                  ..1)
       `(proc-exp ',args ,(parser `(begin ,@body-exps)))]
      [`(,(? λ?) (,(? symbol? #{args : (Listof Symbol)}) ...)
                  ,(? s-exp? #{body-exps : S-List})
                  ..1)
       `(proc-exp ',args ,(parser `(begin ,@body-exps)))]

      [`(,(? trace-λ?) ,(? symbol? args)
                        ,(? s-exp? #{body-exps : S-List})
                        ..1)
       `(trace-proc-exp ',args ,(parser `(begin ,@body-exps)))]
      [`(,(? trace-λ?) (,(? symbol? #{args : (Listof Symbol)}) ...)
                        ,(? s-exp? #{body-exps : S-List})
                        ..1)
       `(trace-proc-exp ',args ,(parser `(begin ,@body-exps)))]

      [`(,(? s-exp? op) ,(? s-exp? #{exps : S-List}) ...)
       `(call-exp ,(parser op) (list ,@(map parser exps)))]

      )))
