#lang racket

(require "../base/base.rkt")


#;(: *check-code* [-> S-Exp Env Namespace Boolean])
(define *check-code*
  (λ (code env eval-ns)
    (with-handlers ([list? (λ (msg)
                             (displayln "Not Equal:")
                             (pretty-print code)
                             (pretty-print msg)
                             (raise #f))]
                    [exn:fail? (λ (ex)
                                 (displayln "Raise Error:")
                                 (pretty-print code)
                                 (raise ex))])
      (define-values (res output)
        (let ()
          (define o (open-output-bytes))
          (values (parameterize ([current-output-port o])
                    (eval code eval-ns))
                  (get-output-bytes o))))

      (define-values (*res* *output*)
        (let ()
          (define *o* (open-output-bytes))
          (values (parameterize ([current-output-port *o*])
                    (*eval* code env (end-cont)))
                  (get-output-bytes *o*))))


      (if (and (equal?  res    *res*)
               (bytes=? output *output*))
          #t
          (raise (list (list res   output)
                       (list *res* *output*)))))))


#;(: init-env [-> Env])
(define init-env
  (λ ()
    #;(extend-env 'i (num-val 1)
                  (extend-env 'v (num-val 5)
                              (extend-env 'x (num-val 10)
                                          (base-env))))
    (extend-env* '(i v x)
                 (list (num-val 1) (num-val 5) (num-val 10))
                 (base-env))))


#;(: base-eval-ns Namespace)
(define base-eval-ns (make-base-namespace))
(namespace-set-variable-value! 'empty-list   (λ () '())   #t base-eval-ns)
(for ([op (in-list (list queue? empty-queue? empty-queue dequeue enqueue))])
  (namespace-set-variable-value! (object-name op) op #t base-eval-ns))
(namespace-set-variable-value! 'Y
                               (λ (f)
                                 ((λ (recur-func)
                                    (recur-func recur-func))
                                  (λ (recur-func)
                                    (f (λ args
                                         (apply (recur-func recur-func) args))))))
                               #t base-eval-ns)
(namespace-set-variable-value! 'Y*
                               (λ funcs
                                 ((λ (recur-funcs)
                                    (recur-funcs recur-funcs))
                                  (λ (recur-funcs)
                                    (map (λ (func)
                                           (λ args
                                             (apply (apply func (recur-funcs recur-funcs)) args)))
                                         funcs))))
                               #t base-eval-ns)


#;(: init-eval-ns Namespace)
(define init-eval-ns (make-base-namespace))
(namespace-set-variable-value! 'empty-list (λ () '()) #t init-eval-ns)
(namespace-set-variable-value! 'Y
                               (λ (f)
                                 ((λ (recur-func)
                                    (recur-func recur-func))
                                  (λ (recur-func)
                                    (f (λ args
                                         (apply (recur-func recur-func) args))))))
                               #t init-eval-ns)
(namespace-set-variable-value! 'Y*
                               (λ funcs
                                 ((λ (recur-funcs)
                                    (recur-funcs recur-funcs))
                                  (λ (recur-funcs)
                                    (map (λ (func)
                                           (λ args
                                             (apply (apply func (recur-funcs recur-funcs)) args)))
                                         funcs))))
                               #t init-eval-ns)

(namespace-set-variable-value! 'i  1 #t init-eval-ns)
(namespace-set-variable-value! 'v  5 #t init-eval-ns)
(namespace-set-variable-value! 'x 10 #t init-eval-ns)


(displayln "Start thread test.\n")


(*check-code* 'x (init-env) init-eval-ns)
(*check-code* 'i (init-env) init-eval-ns)

(*check-code* '(sub1 -9) (init-env) init-eval-ns)
(*check-code* '(sub1 i)  (init-env) init-eval-ns)
(*check-code* '(add1 x)  (init-env) init-eval-ns)

(*check-code* '(> i x) (init-env) init-eval-ns)
(*check-code* '(< i x) (init-env) init-eval-ns)
(*check-code* '(= i x) (init-env) init-eval-ns)

(*check-code* '(cons i i) (init-env) init-eval-ns)
(*check-code* '(car (cons i x)) (init-env) init-eval-ns)
(*check-code* '(list x i i) (init-env) init-eval-ns)
(*check-code* '(null? (empty-list)) (init-env) init-eval-ns)


(*check-code* '2 (base-env) base-eval-ns)
(*check-code* '-9 (base-env) base-eval-ns)

(*check-code* '(not #t) (base-env) base-eval-ns)
(*check-code* '(not #f) (base-env) base-eval-ns)

(*check-code* '#\a (base-env) base-eval-ns)
(*check-code* '"b" (base-env) base-eval-ns)

(*check-code* '(void)     (base-env) base-eval-ns)
(*check-code* '(void 1)   (base-env) base-eval-ns)
(*check-code* '(void 1 2) (base-env) base-eval-ns)

(*check-code* '(cadr   (list 0 1 2 3 4 5 6)) (base-env) base-eval-ns)
(*check-code* '(cdddr  (list 0 1 2 3 4 5 6)) (base-env) base-eval-ns)
(*check-code* '(cadddr (list 0 1 2 3 4 5 6)) (base-env) base-eval-ns)
(*check-code* '(length (list 0 1 2 3)) (base-env) base-eval-ns)

(*check-code* '(boolean? #t) (base-env) base-eval-ns)

(*check-code* '(when (null? (list 1 2 3))
                 'when)
              (base-env) base-eval-ns)
(*check-code* '(when (not (null? (list 1 2 3)))
                 'when)
              (base-env) base-eval-ns)
(*check-code* '(unless (not (null? (list 1 2 3)))
                 'unless)
              (base-env) base-eval-ns)
(*check-code* '(unless (null? (list 1 2 3))
                 'unless)
              (base-env) base-eval-ns)

(*check-code* '(cond [(null? (list 1 2 3)) 'cond-1]
                     [(null? (list 9 0 8)) 'cond-2]
                     [else 'else-cons])
              (base-env) base-eval-ns)
(*check-code* '(cond [(null? (list 1 2 3)) 'cond-1]
                     [(null? (empty-list)) 'cond-2]
                     [else 'else-cons])
              (base-env) base-eval-ns)

(*check-code* '(displayln
                (cond [(null? (list 1 2 3)) 'cond-1]
                      [(null? (empty-list)) 'cond-2]
                      [else 'else-cons]))
              (base-env) base-eval-ns)


(*check-code* '(when (empty-queue? (enqueue (enqueue (enqueue (empty-queue) 1) 2) 3))
                 'when)
              (base-env) base-eval-ns)
(*check-code* '(when (not (empty-queue? (enqueue (enqueue (enqueue (empty-queue) 1) 2) 3)))
                 'when)
              (base-env) base-eval-ns)
(*check-code* '(unless (empty-queue? (enqueue (enqueue (enqueue (empty-queue) 1) 2) 3))
                 'unless)
              (base-env) base-eval-ns)
(*check-code* '(unless (not (empty-queue? (enqueue (enqueue (enqueue (empty-queue) 1) 2) 3)))
                 'unless)
              (base-env) base-eval-ns)

(*check-code* '(cond [(empty-queue? (enqueue (enqueue (enqueue (empty-queue) 1) 2) 3)) 'cond-1]
                     [(empty-queue? (enqueue (enqueue (enqueue (empty-queue) 9) 0) 8)) 'cond-2]
                     [else 'else-cons])
              (base-env) base-eval-ns)
(*check-code* '(cond [(empty-queue? (enqueue (enqueue (enqueue (empty-queue) 1) 2) 3)) 'cond-1]
                     [(null? (empty-list)) 'cond-2]
                     [else 'else-cons])
              (base-env) base-eval-ns)

(*check-code* '(displayln
                (cond [(empty-queue? (enqueue (enqueue (enqueue (empty-queue) 1) 2) 3)) 'cond-1]
                      [(null? (empty-list)) 'cond-2]
                      [else 'else-cons]))
              (base-env) base-eval-ns)


(*check-code* '(let ([x 1])
                 (cons x x))
              (init-env) init-eval-ns)
(*check-code* '(let ([a 1] [b 2] [c 3])
                 (list a b c))
              (init-env) init-eval-ns)

(*check-code* '(let ([x 30])
                 (let ([x (- x 1)]
                       [y (- x 2)])
                   (- x y)))
              (init-env) init-eval-ns)

(*check-code* '(let ([x 30])
                 (let* ([x (- x 1)]
                        [y (- x 2)])
                   (+ x y)
                   (* x y)
                   (- x y)))
              (init-env) init-eval-ns)

(*check-code* '(let ([f (λ (x) (- x 11))])
                 (f (f 77)))
              (base-env) base-eval-ns)
(*check-code* '((λ (f) (f (f 77)))
                (λ (x) (- x 11)))
              (base-env) base-eval-ns)

(*check-code* '(let* ([x 200]
                      [f (λ (z) (- z x))]
                      [x 100]
                      [g (λ (z) (- z x))])
                 (- (f 1) (g 1)))
              (base-env) base-eval-ns)

(*check-code* '((λ args (displayln args))
                1 2 3 4)
              (base-env) base-eval-ns)


(*check-code* '(apply + (list 1 2))
              (base-env) base-eval-ns)

(*check-code* '(apply + '(1 2))
              (base-env) base-eval-ns)

(*check-code* '(let ([fact
                      (Y (λ (fact)
                           (λ (n)
                             (cond [(= n 0) 1]
                                   [(= n 1) 1]
                                   [else (* n (fact (- n 1)))]))))])
                 (fact 5))
              (base-env) base-eval-ns)


(*check-code* '(if #t 1 2)
              (base-env) base-eval-ns)

(*check-code* '(if #f 1 2)
              (base-env) base-eval-ns)


(*check-code* '(null? '((a 0) (b 1) (c 2) (d 3)))
              (base-env) base-eval-ns)

(*check-code* '(map car '((a 0) (b 1) (c 2) (d 3)))
              (base-env) base-eval-ns)

(*check-code* '(apply * `(1 ,(+ 1 2) 4))
              (base-env) base-eval-ns)

(*check-code* '(let ([funcs
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
              (base-env) base-eval-ns)

(*check-code* '(letrec ([even? (λ (num)
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
              (base-env) base-eval-ns)

(*check-code* '(begin
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
              (base-env) base-eval-ns)

(*check-code* '(begin
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
              (base-env) base-eval-ns)

(*check-code* '(begin
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
              (base-env) base-eval-ns)

(*check-code* '(begin
                 (define fib
                   (λ (num)
                     (cond [(= 0 num) 0]
                           [(= 1 num) 1]
                           [else (+ (fib (- num 1))
                                    (fib (- num 2)))])))
                 (fib 2))
              (base-env) base-eval-ns)

(*check-code* '(and) (base-env) base-eval-ns)
(*check-code* '(or) (base-env) base-eval-ns)

(*check-code* '(or #f #f #f) (base-env) base-eval-ns)
(*check-code* '(or #f #t #f) (base-env) base-eval-ns)

(*check-code* '(and #t #t #t) (base-env) base-eval-ns)
(*check-code* '(and #f #t #f) (base-env) base-eval-ns)


(*check-code* '(+ 10 (let/cc cc (+ 1 (cc 2))))
              (base-env) base-eval-ns)
(*check-code* '(let ()
                 (displayln
                  (let/cc cc
                    (displayln 'hello)
                    (cc 'cc)
                    (displayln 'world)))
                 (displayln 456))
              (base-env) base-eval-ns)
(*check-code* '(begin
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
              (base-env) base-eval-ns)

(*check-code* '(+ 10 (call/cc (λ (cc) (+ 1 (cc 2)))) )
              (base-env) base-eval-ns)
(*check-code* '(let ()
                 (displayln
                  (call/cc
                   (λ (cc)
                     (displayln 'hello)
                     (cc 'cc)
                     (displayln 'world))))
                 (displayln 456))
              (base-env) base-eval-ns)
(*check-code* '(begin
                 (define fact
                   (λ (n)
                     (let ([ls (call/cc (λ (cc) (list cc n 1)))])
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
              (base-env) base-eval-ns)
