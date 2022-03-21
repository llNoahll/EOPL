#lang typed/racket

(require "../types/types.rkt")

(provide parser)


(: parser [-> S-Exp S-Exp])
(define parser
  (λ (code)
    (match code
      [`(,(or 'quote 'quasiquote) ,(? symbol? sym))
       `(symbol-exp ',sym)]

      [(? boolean? bool) `(bool-exp ,bool)]
      [(? real? num)     `(real-exp ,num)]
      [(? string? str)   `(string-exp ,str)]
      [(? char? ch)      `(char-exp ,ch)]

      [(? symbol? var)   `(var-exp ',var)]

      [`(set! ,var ,exp)
       #:when (and (symbol? var) (s-exp? exp))
       `(assign-exp ',var ,(parser exp))]

      [`(begin ,exps ..2)
       #:when ((listof? s-exp?) exps)
       `(begin-exp (list ,@(map parser exps)))]

      [`(if ,(? s-exp? pred-exp)
            ,(? s-exp? true-exp)
            ,(? s-exp? false-exp))
       `(if-exp ,(parser pred-exp)
                ,(parser true-exp)
                ,(parser false-exp))]

      [`(with-handlers ([,pred-exps ,handler-exps] ..1)
          ,body-exp)
       #:when (and ((listof? s-exp?) pred-exps)
                   ((listof? s-exp?) handler-exps)
                   (s-exp? body-exp))
       `(handlers-exp (list ,@(map parser pred-exps))
                      (list ,@(map parser handler-exps))
                      ,(parser body-exp))]
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


      [`(letrec ([,bind-vars ,bind-exps] ...) ,body-exp)
       #:when (and ((listof? symbol?) bind-vars)
                   ((listof? s-exp?)  bind-exps)
                   (s-exp? body-exp))
       `(letrec-exp ',bind-vars
                    (list ,@(map parser bind-exps))
                    ,(parser body-exp))]


      [`(,(? λ?) ,args ,body-exp)
       #:when (and ((or/c symbol? (listof? symbol?)) args)
                   (s-exp? body-exp))
       `(proc-exp ',args ,(parser body-exp))]
      [`(,(? trace-λ?) ,args ,body-exp)
       #:when (and ((or/c symbol? (listof? symbol?)) args)
                   (s-exp? body-exp))
       `(trace-proc-exp ',args ,(parser body-exp))]

      [`(,op ,exps ...)
       #:when (and (s-exp? op) ((listof? s-exp?) exps))
       `(call-exp ,(parser op) (list ,@(map parser exps)))]

      )))