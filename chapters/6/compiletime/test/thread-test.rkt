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
        (base-env) (end-cont) 2)

(displayln "\n----------------------------------------------")
(*eval* '(begin
           (define noisy
             (λ (l)
               (cond [(null? l) 0]
                     [else
                      (displayln (car l))
                      (yield)
                      (noisy (cdr l))])))
           (spawn (λ (_) (noisy '(0 1 2 3 4))))
           (spawn (λ (_) (noisy '(5 6 7 8 9))))
           (displayln 100)
           33)
        (base-env) (end-cont) 2)

(displayln "\n----------------------------------------------")
(*eval* '(begin
           (displayln "Start")
           (kill-thread 1)
           (displayln "End"))
        (base-env) (end-cont))

(displayln "\n----------------------------------------------")
(*eval* '(begin
           (define buffer 0)
           (define procedure
             (λ (n)
               (define waitloop
                 (λ (k)
                   (cond [(zero? k) (set! buffer n)]
                         [else
                          (displayln (- k -200))
                          (waitloop (- k 1))])))
               (waitloop 5)))
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
        (base-env) (end-cont) 2)

(displayln "\n----------------------------------------------")
(*eval* '(begin
           (define buffer 0)
           (define procedure
             (λ (n)
               (define waitloop
                 (λ (k)
                   (cond [(zero? k) (set! buffer n)]
                         [else
                          (displayln (- k -200))
                          (yield)
                          (waitloop (- k 1))])))
               (waitloop 5)))
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
        (base-env) (end-cont) 2)

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
        (base-env) (end-cont))

(displayln "\n----------------------------------------------")
(*eval* '(begin
           (displayln "Start")
           (spawn (λ (tid) (thread-send 1 "Hello, World!")))
           (displayln (thread-try-receive))
           (displayln "End"))
        (base-env) (end-cont) 10)

(displayln "\n----------------------------------------------")
(*eval* '(begin
           (displayln "Start")
           (spawn (λ (tid) (thread-send 1 "Hello, World!")))
           (displayln (thread-receive))
           (displayln "End"))
        (base-env) (end-cont))

(displayln "\n----------------------------------------------")
(*eval* '(begin
           (define x 0)
           (define mut (mutex))
           (define incr-x
             (λ (id)
               (λ (tid)
                 (wait mut)
                 (displayln (format "ptid = ~a, tid = ~a, before: x = ~a"
                                    (get-ptid) tid x))
                 (set! x (- x -1))
                 (displayln (format "ptid = ~a, tid = ~a, after: x = ~a"
                                    (get-ptid) tid x))
                 (signal mut))))

           (displayln (format "main thread: ptid = ~a, tid = ~a"
                              (get-ptid) (get-tid)))
           (spawn (incr-x 100))
           (spawn (incr-x 200))
           (spawn (incr-x 300))
           (spawn (incr-x 400))
           (spawn (incr-x 500))
           x)
        (base-env) (end-cont))

(parameterize ([thread-share-memory? #t])
  (displayln "\n----------------------------------------------")
  (*eval* '(begin
             (define x 0)
             (define mut (mutex))
             (define incr-x
               (λ (id)
                 (λ (tid)
                   (wait mut)
                   (displayln (format "ptid = ~a, tid = ~a, before: x = ~a"
                                      (get-ptid) tid x))
                   (set! x (- x -1))
                   (displayln (format "ptid = ~a, tid = ~a, after: x = ~a"
                                      (get-ptid) tid x))
                   (signal mut))))

             (displayln (format "main thread: ptid = ~a, tid = ~a"
                                (get-ptid) (get-tid)))
             (spawn (incr-x 100))
             (spawn (incr-x 200))
             (spawn (incr-x 300))
             (spawn (incr-x 400))
             (spawn (incr-x 500))
             x)
          (base-env) (end-cont)))

(displayln "\n----------------------------------------------")
(*eval* '(begin
           (define x 0)
           (define get-x  (λ () x))
           (define set-x! (λ (v) (set! x v)))
           (define mut (mutex))
           (define incr-x
             (λ (id)
               (λ (tid)
                 (wait mut)
                 (displayln (format "ptid = ~a, tid = ~a, before: x = ~a"
                                    (get-ptid) tid (get-x)))
                 (set-x! (- (get-x) -1))
                 (displayln (format "ptid = ~a, tid = ~a, after: x = ~a"
                                    (get-ptid) tid (get-x)))
                 (signal mut))))

           (displayln (format "main thread: ptid = ~a, tid = ~a"
                              (get-ptid) (get-tid)))
           (spawn (incr-x 100))
           (spawn (incr-x 200))
           (spawn (incr-x 300))
           (spawn (incr-x 400))
           (spawn (incr-x 500))
           (get-x))
        (base-env) (end-cont))

(displayln "\n----------------------------------------------")
(*eval* '(begin
           (define x 0)
           (define mut (mutex 2))
           (define incr-x
             (λ (id)
               (λ (_)
                 (with-mutex mut
                   (displayln (format "ptid = ~a, tid = ~a, before: x = ~a"
                                      (get-ptid) (get-tid) x))
                   (displayln (format "Kill thread ~a: ~a" 5 (kill-thread 5)))
                   (set! x (- x -1))
                   (displayln (format "ptid = ~a, tid = ~a, after: x = ~a"
                                      (get-ptid) (get-tid) x))))))

           (displayln (format "main thread: ptid = ~a, tid = ~a"
                              (get-ptid) (get-tid)))
           (spawn (incr-x 100))
           (spawn (incr-x 200))
           (spawn (incr-x 300))
           (spawn (incr-x 400))
           (spawn (incr-x 500))
           (displayln (format "Kill thread ~a: ~a" 3 (kill-thread 3)))
           x)
        (base-env) (end-cont))

(displayln "\n----------------------------------------------")
(*eval* '(begin
           (define x 0)
           (define get-x  (λ () x))
           (define set-x! (λ (v) (set! x v)))
           (define mut (mutex 2))
           (define incr-x
             (λ (id)
               (λ (_)
                 (with-mutex mut
                   (displayln (format "ptid = ~a, tid = ~a, before: x = ~a"
                                      (get-ptid) (get-tid) (get-x)))
                   (displayln (format "Kill thread ~a: ~a" 5 (kill-thread 5)))
                   (set-x! (- (get-x) -1))
                   (displayln (format "ptid = ~a, tid = ~a, after: x = ~a"
                                      (get-ptid) (get-tid) (get-x)))))))

           (displayln (format "main thread: ptid = ~a, tid = ~a"
                              (get-ptid) (get-tid)))
           (spawn (incr-x 100))
           (spawn (incr-x 200))
           (spawn (incr-x 300))
           (spawn (incr-x 400))
           (spawn (incr-x 500))
           (displayln (format "Kill thread ~a: ~a" 3 (kill-thread 3)))
           (get-x))
        (base-env) (end-cont))
