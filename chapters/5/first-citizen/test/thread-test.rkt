#lang typed/racket

(require "../base/base.rkt")

(displayln "Start thread test.\n")

(displayln "\n----------------------------------------------")
(*eval* '(begin
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
        (base-env) (end-main-thread-cont))

(displayln "\n----------------------------------------------")
(*eval* '(begin
           (define buffer 0)
           (define procedure
             (λ (n)
               (define justwait
                 (λ (k)
                   (cond [(zero? k) (set! buffer n)]
                         [else
                          (displayln (- k -200))
                          (justwait (- k 1))])))
               (justwait 5)))
           (define consumer
             (λ (d)
               (define busywait
                 (λ (k)
                   (cond [(zero? buffer)
                          (displayln (- k -100))
                          (busywait (- k -1))]
                         [else buffer])))
               (busywait 0)))

           (spawn (λ (_) (procedure 44)))
           (displayln 300)
           (consumer 86))
        (base-env) (end-main-thread-cont))

(displayln "\n----------------------------------------------")
(*eval* '(begin
           (define x 0)
           (define incr-x (λ (id) (λ (_) (set! x (- x -1)))))

           (spawn (incr-x 100))
           (spawn (incr-x 200))
           (spawn (incr-x 300))
           (spawn (incr-x 400))
           (spawn (incr-x 500))
           x)
        (base-env) (end-main-thread-cont))

(displayln "\n----------------------------------------------")
(*eval* '(begin
           (define x 0)
           (define mut (mutex))
           (define incr-x
             (λ (id)
               (λ (_)
                 (wait mut)
                 (displayln (format "before: x = ~a" x))
                 (set! x (- x -1))
                 (displayln (format "after: x = ~a" x))
                 (signal mut))))

           (spawn (incr-x 100))
           (spawn (incr-x 200))
           (spawn (incr-x 300))
           (spawn (incr-x 400))
           (spawn (incr-x 500))
           x)
        (base-env) (end-main-thread-cont))

(displayln "\n----------------------------------------------")
(*eval* '(begin
           (define x 0)
           (define mut (mutex 2))
           (define incr-x
             (λ (id)
               (λ (_)
                 (with-mutex mut
                   (displayln (format "before: x = ~a" x))
                   (set! x (- x -1))
                   (displayln (format "after: x = ~a" x))))))

           (spawn (incr-x 100))
           (spawn (incr-x 200))
           (spawn (incr-x 300))
           (spawn (incr-x 400))
           (spawn (incr-x 500))
           x)
        (base-env) (end-main-thread-cont))
