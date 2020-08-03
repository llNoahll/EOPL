#lang typed/racket

(require "../base/base.rkt"
         "repl.rkt")


(: init-env [-> Env])
(define init-env
  (λ ()
    ;; (extend-env 'i (num-val 1)
    ;;             (extend-env 'v (num-val 5)
    ;;                         (extend-env 'x (num-val 10)
    ;;                                     (base-env))))
    (extend-env* '(i v x)
                 (list (num-val 1) (num-val 5) (num-val 10))
                 (base-env))))


(displayln (*eval* '2 (base-env)))
(displayln (*eval* '-9 (base-env)))

(displayln (*eval* 'x (init-env)))
(displayln (*eval* 'i (init-env)))

(displayln (*eval* '#\a (base-env)))
(displayln (*eval* '"b" (base-env)))

(displayln (*eval* '(minus -9) (init-env)))
(displayln (*eval* '(minus i) (init-env)))
(displayln (*eval* '(minus x) (init-env)))

(displayln (*eval* '(> i x) (init-env)))
(displayln (*eval* '(< i x) (init-env)))
(displayln (*eval* '(= i x) (init-env)))

(displayln (*eval* '(cons i i) (init-env)))
(displayln (*eval* '(car (cons i x)) (init-env)))
(displayln (*eval* '(list x i i) (init-env)))
(displayln (*eval* '(null? (empty-list)) (init-env)))
(displayln (*eval* '(cond [(null? (list 1 2 3)) 'cond-1]
                          [(null? (list 9 0 8)) 'cond-2]
                          [else 'else-cons])
                   (base-env)))
(displayln (*eval* '(cond [(null? (list 1 2 3)) 'cond-1]
                          [(null? (empty-list)) 'cond-2]
                          [else 'else-cons])
                   (base-env)))

(displayln (*eval* '(displayln (cond [(null? (list 1 2 3)) 'cond-1]
                                     [(null? (empty-list)) 'cond-2]
                                     [else 'else-cons]))
                   (base-env)))


(displayln (*eval* '(let ([x 1])
                      (cons x x))
                   (init-env)))
(displayln (*eval* '(let ([a 1]
                          [b 2]
                          [c 3])
                      (list a b c))
                   (init-env)))

(displayln (*eval* '(let ([x 30])
                      (let ([x (- x 1)]
                            [y (- x 2)])
                        (- x y)))
                   (init-env)))

(displayln (*eval* '(let ([x 30])
                      (let* ([x (- x 1)]
                             [y (- x 2)])
                        (+ x y)
                        (* x y)
                        (- x y)))
                   (init-env)))

(displayln (*eval* '(let ([f (λ (x) (- x 11))])
                      (f (f 77)))
                   (base-env)))
(displayln (*eval* '((λ (f) (f (f 77)))
                     (λ (x) (- x 11)))
                   (base-env)))

(displayln (*eval* '(let* ([x 200]
                           [f (λ (z) (- z x))]
                           [x 100]
                           [g (λ (z) (- z x))])
                      (- (f 1) (g 1)))
                   (base-env)))

(displayln (*eval* '((λ args (displayln args))
                     1 2 3 4)
                   (base-env)))


(displayln (*eval* '(apply + (list 1 2))
                   (base-env)))

(displayln (*eval* '(apply + '(1 2))
                   (base-env)))

(displayln (*eval* '(let ([fact
                           (Y (λ (fact)
                                (λ (n)
                                  (cond [(= n 0) 1]
                                        [(= n 1) 1]
                                        [else (* n (fact (- n 1)))]))))])
                      (fact 5))
                   (base-env)))


(displayln (*eval* '(let ([funcs
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
                   (base-env)))



;; (*repl* (base-env))
