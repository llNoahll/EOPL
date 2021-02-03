#lang racket

(require "../base/base.rkt")


;; (: *check-code* [-> S-Exp Env Namespace Boolean])
(define *check-code*
  (λ (code env eval-ns)
    (equal? (*eval* code env)
            (eval code eval-ns))))


;; (: init-env [-> Env])
(define init-env
  (λ ()
    ;; (extend-env 'i (num-val 1)
    ;;             (extend-env 'v (num-val 5)
    ;;                         (extend-env 'x (num-val 10)
    ;;                                     (base-env))))
    (extend-env* '(i v x)
                 (list (num-val 1) (num-val 5) (num-val 10))
                 (base-env))))


;; (: base-eval-ns Namespace)
(define base-eval-ns (make-base-namespace))
(namespace-set-variable-value! 'empty-list (λ () '()) #t base-eval-ns)
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


;; (: init-eval-ns Namespace)
(define init-eval-ns (make-base-namespace))
(namespace-set-variable-value! 'empty-list (λ () '()) #t init-eval-ns)
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

(namespace-set-variable-value! 'i  1 #t init-eval-ns)
(namespace-set-variable-value! 'v  5 #t init-eval-ns)
(namespace-set-variable-value! 'x 10 #t init-eval-ns)


(displayln (*check-code* '2 (base-env) base-eval-ns))
(displayln (*check-code* '-9 (base-env) base-eval-ns))

(displayln (*check-code* '(not #t) (base-env) base-eval-ns))
(displayln (*check-code* '(not #f) (base-env) base-eval-ns))

(displayln (*check-code* 'x (init-env) init-eval-ns))
(displayln (*check-code* 'i (init-env) init-eval-ns))

(displayln (*check-code* '#\a (base-env) base-eval-ns))
(displayln (*check-code* '"b" (base-env) base-eval-ns))

(displayln (*check-code* '(sub1 -9) (init-env) init-eval-ns))
(displayln (*check-code* '(sub1 i) (init-env) init-eval-ns))
(displayln (*check-code* '(add1 x) (init-env) init-eval-ns))

(displayln (*check-code* '(> i x) (init-env) init-eval-ns))
(displayln (*check-code* '(< i x) (init-env) init-eval-ns))
(displayln (*check-code* '(= i x) (init-env) init-eval-ns))

(displayln (*check-code* '(cons i i) (init-env) init-eval-ns))
(displayln (*check-code* '(car (cons i x)) (init-env) init-eval-ns))
(displayln (*check-code* '(list x i i) (init-env) init-eval-ns))
(displayln (*check-code* '(null? (empty-list)) (init-env) init-eval-ns))
(displayln (*check-code* '(cond [(null? (list 1 2 3)) 'cond-1]
                                [(null? (list 9 0 8)) 'cond-2]
                                [else 'else-cons])
                         (base-env) base-eval-ns))
(displayln (*check-code* '(cond [(null? (list 1 2 3)) 'cond-1]
                                [(null? (empty-list)) 'cond-2]
                                [else 'else-cons])
                         (base-env) base-eval-ns))

(displayln (*check-code* '(displayln (cond [(null? (list 1 2 3)) 'cond-1]
                                           [(null? (empty-list)) 'cond-2]
                                           [else 'else-cons]))
                         (base-env) base-eval-ns))


(displayln (*check-code* '(let ([x 1])
                            (cons x x))
                         (init-env) init-eval-ns))
(displayln (*check-code* '(let ([a 1]
                                [b 2]
                                [c 3])
                            (list a b c))
                         (init-env) init-eval-ns))

(displayln (*check-code* '(let ([x 30])
                            (let ([x (- x 1)]
                                  [y (- x 2)])
                              (- x y)))
                         (init-env) init-eval-ns))

(displayln (*check-code* '(let ([x 30])
                            (let* ([x (- x 1)]
                                   [y (- x 2)])
                              (+ x y)
                              (* x y)
                              (- x y)))
                         (init-env) init-eval-ns))

(displayln (*check-code* '(let ([f (λ (x) (- x 11))])
                            (f (f 77)))
                         (base-env) base-eval-ns))
(displayln (*check-code* '((λ (f) (f (f 77)))
                           (λ (x) (- x 11)))
                         (base-env) base-eval-ns))

(displayln (*check-code* '(let* ([x 200]
                                 [f (λ (z) (- z x))]
                                 [x 100]
                                 [g (λ (z) (- z x))])
                            (- (f 1) (g 1)))
                         (base-env) base-eval-ns))

(displayln (*check-code* '((λ args (displayln args))
                           1 2 3 4)
                         (base-env) base-eval-ns))


(displayln (*check-code* '(apply + (list 1 2))
                         (base-env) base-eval-ns))

(displayln (*check-code* '(apply + '(1 2))
                         (base-env) base-eval-ns))

(displayln (*check-code* '(let ([fact
                                 (Y (λ (fact)
                                      (λ (n)
                                        (cond [(= n 0) 1]
                                              [(= n 1) 1]
                                              [else (* n (fact (- n 1)))]))))])
                            (fact 5))
                         (base-env) base-eval-ns))


(displayln (*check-code* '(if #t 1 2)
                         (base-env) base-eval-ns))

(displayln (*check-code* '(if #f 1 2)
                         (base-env) base-eval-ns))


(displayln (*check-code* '(null? '((a 0) (b 1) (c 2) (d 3)))
                         (base-env) base-eval-ns))

(displayln (*check-code* '(map car '((a 0) (b 1) (c 2) (d 3)))
                         (base-env) base-eval-ns))

(displayln (*check-code* '(let ([funcs
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
                         (base-env) base-eval-ns))

(displayln (*check-code* '(letrec ([even? (λ (num)
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
                         (base-env) base-eval-ns))

(displayln (*check-code* '(begin
                            (define fib
                              (λ (num)
                                (cond [(= 0 num) 0]
                                      [(= 1 num) 1]
                                      [else (+ (fib (- num 1))
                                               (fib (- num 2)))])))
                            (fib 2))
                         (base-env) base-eval-ns))

(displayln (*check-code* '(and) (base-env) base-eval-ns))
(displayln (*check-code* '(or) (base-env) base-eval-ns))

(displayln (*check-code* '(or #f #f #f) (base-env) base-eval-ns))
(displayln (*check-code* '(or #f #t #f) (base-env) base-eval-ns))

(displayln (*check-code* '(and #t #t #t) (base-env) base-eval-ns))
(displayln (*check-code* '(and #f #t #f) (base-env) base-eval-ns))

;; (*repl* (base-env))
