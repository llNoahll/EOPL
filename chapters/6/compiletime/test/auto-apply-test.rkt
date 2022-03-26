#lang typed/racket

(require "../Parse/desugar.rkt"
         "../Parse/auto-apply.rkt")

(displayln "Start auto apply test.\n")

(for ([code
       (in-list
        '(+
          (+)
          (+ 0)
          (+ 0 1)
          (+ 0 1 2)
          (+ 0 1 2 3)
          (+ 0 1 2 3 4)
          (apply + (list 0 1 2))
          (apply + '(0 1 2))
          (begin
            (define noisy
              (λ (l)
                (cond [(null? l) 0]
                      [else
                       (displayln (car l))
                       (noisy (cdr l))])))
            (spawn (λ (_) (noisy '(0 1 2 3 4))))
            (spawn (λ (_) (noisy '(5 6 7 8 9))))
            (displayln 100)
            33)
          (begin
            (define fact
              (λ (n)
                (let ([ls (let/cc cc (list cc n 1))])
                  (define cc  (car ls))
                  (define n   (car (cdr ls)))
                  (define res (car (cdr (cdr ls))))
                  (if (zero? n)
                      res
                      (cc (list cc (sub1 n) (* n res)))))))

            (list (fact 0)
                  (fact 1)
                  (fact 2)
                  (fact 3)
                  (fact 4)
                  (fact 5)))
          (let ([n 0]) (set! n (+ n 1)) n)
          (+ 10 (let/cc cc (+ 1 (cc 2))))
          (let ()
            (displayln
             (call/cc
              (λ (cc)
                (displayln 'hello)
                (cc 'cc)
                (displayln 'world))))
            (displayln 456))
          (let ([n 3])
            (with-handlers ([(λ (arg) (eq? arg 2))
                             (λ (arg) (displayln "raise a value: TWO."))]
                            [(λ (arg) (eq? arg 3))
                             (λ (arg) (displayln "raise a value: THREE."))])
              (displayln "1st handlers start")
              (with-handlers ([(λ (arg) (eq? arg 0))
                               (λ (arg) (displayln "raise a value: ZERO."))]
                              [(λ (arg) (eq? arg 1))
                               (λ (arg) (displayln "raise a value: ONE."))])
                (displayln "2nd handlers start")
                (raise n)
                (displayln "2nd handlers end"))
              (displayln "1st handlers end")))
          (begin
            (displayln "Start")
            (spawn (λ (tid) (thread-send 1 "Hello, World!")))
            (displayln (thread-try-receive))
            (displayln "End"))
          (begin
            (displayln "Start")
            (spawn (λ (tid) (thread-send 1 "Hello, World!")))
            (displayln (thread-receive))
            (displayln "End"))
          ))])
  (displayln "\n----------------------------------------------")
  (pretty-print
   (desugar
    (auto-apply
     (desugar
      code)))))
