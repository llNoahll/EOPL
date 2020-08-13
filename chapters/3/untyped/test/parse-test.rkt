#lang racket

(require "../Parse/parser.rkt")


(pretty-print (parser '2))
(pretty-print (parser '-9))
(pretty-print (parser 'x))
(pretty-print (parser 'i))

(pretty-print (parser '#\a))
(pretty-print (parser '"b"))

(pretty-print (parser '(minus -9)))
(pretty-print (parser '(minus i)))
(pretty-print (parser '(minus x)))

(pretty-print (parser '(greater? i x)))
(pretty-print (parser '(less? i x)))

(pretty-print (parser '(cons i i)))
(pretty-print (parser '(car (cons i x))))
(pretty-print (parser '(list x i i)))
(pretty-print (parser '(null? (empty-list))))

(pretty-print (parser '(let ([x 1])
                         (cons x x))))

(pretty-print (parser '(let ([a 'a]
                             [b 'b]
                             [c 'c])
                         (list a b c))))


(pretty-print (parser '(if (greater? 2 1) 'true 'false)))

(pretty-print (parser '(cond [(null? (list 1 2 3)) 'cond-1]
                             [(null? (list 9 0 8)) 'cond-2]
                             [else 'else-cons])))


(pretty-print (parser '(let ([x 30])
                         (let* ([x (- x 1)]
                                [y (- x 2)])
                           (+ x y)
                           (* x y)
                           (- x y)))))

(pretty-print (parser '(let* ([a 1]
                              [b 2]
                              [c 3])
                         (list a b c))))

(pretty-print (parser '(let ([x 30])
                         (let* ([x (- x 1)]
                                [y (- x 2)])
                           (- x y)))))


(pretty-print (parser '(let ([f (λ (x) (- x 11))])
                         (f (f 77)))))

(pretty-print (parser ''a))
(pretty-print (parser ''#t))
(pretty-print (parser ''233))
(pretty-print (parser ''"hello"))
(pretty-print (parser ''#\b))
(pretty-print (parser ''(1 2 3 'a "cd")))


(pretty-print (parser '(λ (f)
                         ((λ (recur-func)
                            (recur-func recur-func))
                          (λ (recur-func)
                            (f (λ args
                                 (apply (recur-func recur-func) args))))))))


(pretty-print (parser '(cond [(= n 0) 1]
                             [(= n 1) 1]
                             [else (* n (fact (sub1 n)))])))
(pretty-print (parser '(λ (fact)
                         (λ (n)
                           (cond [(= n 0) 1]
                                 [(= n 1) 1]
                                 [else (* n (fact (sub1 n)))])))))


(pretty-print (parser '(let ([funcs
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
                           (displayln (eq? #t (even? 0)))))))


(pretty-print (parser '(λ funcs
                         ((λ (recur-funcs)
                            (displayln recur-funcs)
                            (displayln (recur-funcs recur-funcs))

                            (recur-funcs recur-funcs))
                          (λ (recur-funcs)
                            (map (λ (func)
                                   (λ args
                                     (apply (apply func (recur-funcs recur-funcs)) args)))
                                 funcs))))))


(pretty-print (parser '(if #t 1 2)))
