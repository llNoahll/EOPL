#lang typed/racket

(require "parser.rkt")


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
                         (let ([x (- x 1)]
                               [y (- x 2)])
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