#lang racket

(require "../types/types.rkt")

(provide (all-defined-out))


(define parser
  (λ (code)
    (match code
      ['() `(empty-list)]
      [`(quote ,(? symbol? symbol)) `(symbol-exp ',symbol)]
      [`(quote ,(? boolean? bool)) `(bool-exp ,bool)]
      [`(quote ,(? real? num)) `(const-exp ,num)]
      [`(quote ,(? string? str)) `(string-exp ,str)]
      [`(quote ,(? char? ch)) `(char-exp ,ch)]
      [`(quote ,(? s-list? ls))
       (parser `(list ,@(map (λ (arg)
                               `(quote ,arg))
                             ls)))]

      [(? boolean? bool) `(bool-exp ,bool)]
      [(? real? num) `(const-exp ,num)]
      [(? string? str) `(string-exp ,str)]
      [(? char? ch) `(char-exp ,ch)]

      [(? symbol? var) `(var-exp ',var)]

      [`(begin ,(? s-exp? exps) ...)
       `(begin-exp
          (list ,@(map parser exps)))]

      [`(if ,(? s-exp? pred-exp)
            ,(? s-exp? true-exp)
            ,(? s-exp? false-exp))
       `(if-exp ,(parser pred-exp)
                ,(parser true-exp)
                ,(parser false-exp))]
      [`(cond [,(? s-exp? pred-exps)
               ,(? s-exp? body-exps)
               ...]
              ...)
       `(cond-exp
         (list ,@(map (λ (pred-exp body-exps)
                        `(list ,(parser (if (eq? pred-exp 'else)
                                            #t
                                            pred-exp))
                               ,(parser `(begin ,@body-exps))))
                      pred-exps
                      body-exps)))]
      [`(let ([,(? symbol? bound-vars)
               ,(? s-exp?  bound-exps)]
              ...)
          ,(? s-exp? body-exps)
          ...)
       `(let-exp ',bound-vars
                 (list ,@(map parser bound-exps))
                 ,(parser `(begin ,@body-exps)))]
      [`(let* ([,(? symbol? bound-vars)
                ,(? s-exp?  bound-exps)]
               ...)
          ,(? s-exp? body-exps)
          ...)
       (parser
        (if (and (null? bound-vars) (null? bound-exps))
            `(let () ,@body-exps)
            `(let ([,(car bound-vars) ,(car bound-exps)])
               (let* (,@(map (λ (var exp)
                               (list var exp))
                             (cdr bound-vars)
                             (cdr bound-exps)))
                 ,@body-exps))))]

      [`(,(? lambda?) ,(? symbol? args)
                      ,(? s-exp? body-exps)
                      ...)
       `(proc-exp ',args ,(parser `(begin ,@body-exps)))]
      [`(,(? lambda?) (,(? symbol? args) ...)
                      ,(? s-exp? body-exps)
                      ...)
       `(proc-exp ',args ,(parser `(begin ,@body-exps)))]
      [`(,(? s-exp? op) ,(? s-exp? exps) ...)
       `(call-exp ,(parser op)
                  (list ,@(map parser exps)))]
      )))
