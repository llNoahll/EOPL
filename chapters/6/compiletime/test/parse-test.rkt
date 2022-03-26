#lang typed/racket

(require "../Parse/parse.rkt")


(for ([i (in-naturals)]
      [code
       (in-list
        '(2
          -9
          (not #t)
          (not #f)
          #\a
          "b"
          (void)
          (void 1)
          (void 1 2)
          (cadr   (list 0 1 2 3 4 5 6))
          (cdddr  (list 0 1 2 3 4 5 6))
          (cadddr (list 0 1 2 3 4 5 6))
          (length (list 0 1 2 3))
          (cadr   '(0 1 2 3 4 5 6))
          (cdddr  '(0 1 2 3 4 5 6))
          (cadddr '(0 1 2 3 4 5 6))
          (length '(0 1 2 3))
          (boolean? #t)
          (when (null? (list 1 2 3)) 'when)
          (when (not (null? (list 1 2 3))) 'when)
          (unless (not (null? (list 1 2 3))) 'unless)
          (unless (null? (list 1 2 3)) 'unless)
          (cond [(null? (list 1 2 3)) 'cond-1]
                [(null? (list 9 0 8)) 'cond-2]
                [else 'else-cons])
          (cond [(null? (list 1 2 3)) 'cond-1]
                [(null? (empty-list)) 'cond-2]
                [else 'else-cons])
          (displayln
           (cond [(null? (list 1 2 3)) 'cond-1]
                 [(null? (empty-list)) 'cond-2]
                 [else 'else-cons]))

          (when (empty-queue? (enqueue (enqueue (enqueue (empty-queue) 1) 2) 3)) 'when)
          (when (not (empty-queue? (enqueue (enqueue (enqueue (empty-queue) 1) 2) 3))) 'when)
          (unless (empty-queue? (enqueue (enqueue (enqueue (empty-queue) 1) 2) 3)) 'unless)
          (unless (not (empty-queue? (enqueue (enqueue (enqueue (empty-queue) 1) 2) 3))) 'unless)
          (cond [(empty-queue? (enqueue (enqueue (enqueue (empty-queue) 1) 2) 3)) 'cond-1]
                [(empty-queue? (enqueue (enqueue (enqueue (empty-queue) 9) 0) 8)) 'cond-2]
                [else 'else-cons])
          (cond [(empty-queue? (enqueue (enqueue (enqueue (empty-queue) 1) 2) 3)) 'cond-1]
                [(null? (empty-list)) 'cond-2]
                [else 'else-cons])
          (displayln
           (cond [(empty-queue? (enqueue (enqueue (enqueue (empty-queue) 1) 2) 3)) 'cond-1]
                 [(null? (empty-list)) 'cond-2]
                 [else 'else-cons]))

          (let ([x 1]) (cons x x))
          (let ([a 1] [b 2] [c 3])
            (list a b c))
          (let ([x 30])
            (let ([x (- x 1)]
                  [y (- x 2)])
              (- x y)))
          (let ([x 30])
            (let* ([x (- x 1)]
                   [y (- x 2)])
              (+ x y)
              (* x y)
              (- x y)))
          (let ([f (λ (x) (- x 11))])
            (f (f 77)))
          ((λ (f) (f (f 77)))
           (λ (x) (- x 11)))
          (let* ([x 200]
                 [f (λ (z) (- z x))]
                 [x 100]
                 [g (λ (z) (- z x))])
            (- (f 1) (g 1)))
          ((λ args (displayln args))
           1 2 3 4)

          (apply + (list 1 2))
          (apply + '(1 2))
          (let ([fact
                 (Y (λ (fact)
                      (λ (n)
                        (cond [(= n 0) 1]
                              [(= n 1) 1]
                              [else (* n (fact (- n 1)))]))))])
            (fact 5))

          (if #t 1 2)
          (if #f 1 2)
          (null? '((a 0) (b 1) (c 2) (d 3)))
          (map car '((a 0) (b 1) (c 2) (d 3)))
          (apply * `(1 ,(+ 1 2) 4))

          (let ([funcs
                 (Y*
                  (λ (even? odd?)
                    (λ (num)
                      (cond [(zero? num) #t]
                            [(= 1 num) #f]
                            [else (odd? (- num 1))])))
                  (λ (even? odd?)
                    (λ (num)
                      (cond [(zero? num) #f]
                            [(= 1 num) #t]
                            [else (even? (- num 1))]))))])
            (let ([even? (car funcs)]
                  [odd?  (car (cdr funcs))])
              (displayln (eq? #t (even? 0)))))
          (letrec ([even? (λ (num)
                            (cond [(zero? num) #t]
                                  [(= 1 num) #f]
                                  [else (odd? (sub1 num))]))]
                   [odd?  (λ (num)
                            (cond [(zero? num) #f]
                                  [(= 1 num) #t]
                                  [else (even? (sub1 num))]))])
            (displayln "-----------------------")
            (displayln (even? 0))
            (displayln (even? 2))
            (displayln (even? 4))

            (displayln (odd? 1))
            (displayln (odd? 3))
            (displayln (odd? 5)))
          (begin
            (define odd?
              (λ (num)
                (cond [(zero? num) #f]
                      [(= 1 num) #t]
                      [else (even? (sub1 num))])))
            (define even?
              (λ (num)
                (cond [(zero? num) #t]
                      [(= 1 num) #f]
                      [else (odd? (sub1 num))])))
            (displayln "-----------------------")
            (displayln (even? 0))
            (displayln (even? 2))
            (displayln (even? 4))

            (displayln (odd? 1))
            (displayln (odd? 3))
            (displayln (odd? 5)))

          (begin
            (define sqrt
              (λ (x)
                (define average
                  (λ (x y)
                    (/ (+ x y) 2)))
                (define abs
                  (λ (x)
                    (cond [(< x 0) (- x)]
                          [(= x 0) 0]
                          [(> x 0) x])))
                (define good-enough?
                  (λ (y)
                    (< (abs (- (* y y) x)) tolerance)))
                (define improve
                  (λ (y)
                    (average (/ x y) y)))
                (define try
                  (λ (y)
                    (if (good-enough? y)
                        y
                        (try (improve y)))))

                (define tolerance 0.0000001)

                (try 1)))
            (sqrt 2))
          (begin
            (define (sqrt x)
              (define (average x y) (/ (+ x y) 2))
              (define (improve y) (average (/ x y) y))
              (define (abs x)
                (cond [(< x 0) (- x)]
                      [(= x 0) 0]
                      [(> x 0) x]))
              (define (good-enough? y)
                (< (abs (- (* y y) x)) tolerance))
              (define (try y)
                (if (good-enough? y)
                    y
                    (try (improve y))))

              (define tolerance 0.0000001)

              (try 1))
            (sqrt 2))

          (begin
            (define fib
              (λ (num)
                (cond [(= 0 num) 0]
                      [(= 1 num) 1]
                      [else (+ (fib (- num 1))
                               (fib (- num 2)))])))
            (fib 2))

          (and)
          (or)
          (or #f #f #f)
          (or #f #t #f)
          (and #t #t #t)
          (and #f #t #f)

          (+ 10 (let/cc cc (+ 1 (cc 2))))
          (let ()
            (displayln
             (let/cc cc
               (displayln 'hello)
               (cc 'cc)
               (displayln 'world)))
            (displayln 456))
          (begin
            (define fact
              (λ (n)
                (let ([ls (let/cc cc (list cc n 1))])
                  (define cc  (car   ls))
                  (define n   (cadr  ls))
                  (define res (caddr ls))
                  (if (zero? n)
                      res
                      (cc (list cc (sub1 n) (* n res)))))))

            (list (fact 0)
                  (fact 1)
                  (fact 2)
                  (fact 3)
                  (fact 4)
                  (fact 5)))
          (+ 10 (call/cc (λ (cc) (+ 1 (cc 2)))) )
          (let ()
            (displayln
             (call/cc
              (λ (cc)
                (displayln 'hello)
                (cc 'cc)
                (displayln 'world))))
            (displayln 456))
          (begin
            (define fact
              (λ (n)
                (let ([ls (call/cc (λ (cc) (list cc n 1)))])
                  (define cc  (car   ls))
                  (define n   (cadr  ls))
                  (define res (caddr ls))
                  (if (zero? n)
                      res
                      (cc (list cc (sub1 n) (* n res)))))))

            (list (fact 0)
                  (fact 1)
                  (fact 2)
                  (fact 3)
                  (fact 4)
                  (fact 5)))
          ))])
  (displayln (format "initial ~a:" i))
  (pretty-print code)
  (displayln (format "desugar ~a:" i))
  (pretty-print (desugar
                 code))
  (displayln (format "auto-applied ~a:" i))
  (pretty-print (auto-apply
                 (desugar
                  code)))
  (displayln (format "auto-cpsed ~a:" i))
  (pretty-print (auto-cps
                 (desugar
                  (auto-apply
                   (desugar
                    code)))))
  (displayln (format "parse ~a:" i))
  (pretty-print (parse code))
  (newline))