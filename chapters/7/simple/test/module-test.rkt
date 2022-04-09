#lang racket

(require "../Parse/parse.rkt"
         "../Modules/thread.rkt"
         "../Modules/exit.rkt")


(define-namespace-anchor ns-anchor)
(define eval-ns (namespace-anchor->namespace ns-anchor))

(for ([i (in-naturals)]
      [code
       (in-list
        '((begin
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
          (begin
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
          (begin
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
            x))
        )])

  (define insert-dequeue
    (λ (code)
      `(begin
         (define dequeue
           (λ (q f)
             (cond
               [(empty-queue? q)
                (raise "ERROR! Passing empty queue to dequeue!")]
               [(null? (cadr q))
                (let ([l (reverse (car q))])
                  (f (car l) (list null (cdr l))))]
               [else
                (let ([l (cadr q)])
                  (f (car l) (list (car q) (cdr l))))])))
         ,code)))

  (define insert-thread
    (λ (code)
      `(let/cc exit
         (define make-thd
           (λ (ptid tid mail time-slice thunk)
             (vector-immutable 'thd ptid tid mail time-slice thunk)))

         (define thd?
           (λ (arg)
             (and (vector? arg)
                  (immutable? arg)
                  (eqv? 'thd (vector-ref arg 0)))))

         (define thd-ptid       (λ (arg) (if (thd? arg) (vector-ref arg 1) #f)))
         (define thd-tid        (λ (arg) (if (thd? arg) (vector-ref arg 2) #f)))
         (define thd-mail       (λ (arg) (if (thd? arg) (vector-ref arg 3) #f)))
         (define thd-time-slice (λ (arg) (if (thd? arg) (vector-ref arg 4) #f)))
         (define thd-thunk      (λ (arg) (if (thd? arg) (vector-ref arg 5) #f)))


         (define thread-table (make-hasheq (list (cons 0 #t))))
         (define mail (box (empty-queue)))
         (define ptid 0)
         (define tid  0)
         (define ntid 0)

         (define initialize-thread-identifier!
           (λ ()
             (hash-clear! thread-table)
             (hash-set! thread-table 0 #t)
             (set-box! mail (empty-queue))
             (set! ptid 0)
             (set! tid  0)
             (set! ntid 0)))

         (define update-thread-identifier!
           (λ (th)
             (set! ptid (thd-ptid th))
             (set! tid  (thd-tid  th))
             (set! mail (thd-mail th))
             (hash-set! thread-table tid #t)))


         (define get-mail (λ () mail))
         (define get-ptid (λ () ptid))
         (define get-tid  (λ () tid))
         (define get-ntid (λ () (set! ntid (add1 ntid)) ntid))

         (define has-thread? (λ (tid) (hash-has-key? thread-table tid)))
         (define get-thread  (λ (tid) (hash-ref thread-table tid #f)))
         (define add-thread! (λ (tid th) (hash-set! thread-table tid th)))

         (define apply-thd (λ (th) (update-thread-identifier! th) ((thd-thunk th))))


         (define the-ready-queue    (empty-queue))
         (define the-final-answer   undefined)
         (define the-max-time-slice 1)
         (define the-time-remaining 0)

         (define initialize-scheduler!
           (λ (ticks)
             (set! the-ready-queue    (empty-queue))
             (set! the-final-answer   undefined)
             (set! the-max-time-slice ticks)
             (set! the-time-remaining the-max-time-slice)))


         (define place-on-thread-queue
           (λ (thds thk ptid tid mail)
             (add-thread! tid
                          (make-thd ptid tid mail
                                    (let ([the-time the-time-remaining])
                                      (if (exact-positive-integer? the-time)
                                          the-time
                                          the-max-time-slice))
                                    thk))
             (enqueue thds tid)))

         (define place-on-ready-queue!
           (λ (thk ptid tid mail)
             (set! the-ready-queue
                   (place-on-thread-queue
                    the-ready-queue
                    thk ptid tid mail))))


         (define set-final-answer! (λ (val) (set! the-final-answer val)))
         (define time-expired?     (λ () (= 0 the-time-remaining)))
         (define decrement-timer!  (λ () (set! the-time-remaining (sub1 the-time-remaining))))

         (define run-next-thread
           (λ ()
             (exit
              (if (empty-queue? the-ready-queue)
                  the-final-answer
                  (dequeue the-ready-queue
                           (λ (1st-ready-tid other-ready-tids)
                             (let ([th (get-thread 1st-ready-tid)])
                               (set! the-ready-queue other-ready-tids)
                               (when (thd? th)
                                 (set! the-time-remaining (thd-time-slice th))
                                 (let ([res (apply-thd th)])
                                   (when (= 1 (get-tid))
                                     (set-final-answer! res))))
                               (run-next-thread))))))))


         (define kill-thread
           (λ (tid)
             (let ([th (get-thread tid)])
               (cond [(false? th) #f]
                     [else
                      (hash-remove! thread-table tid)
                      (if (thd? th) #t (void))]))))

         (define thread-send
           (λ (tid v)
             (if (has-thread? tid)
                 (let* ([th (get-thread tid)]
                        [mail (if (thd? th) (thd-mail th) (get-mail))])
                   (set-box! mail (enqueue (unbox mail) v)))
                 (get-tid))))

         (define thread-try-receive
           (λ ()
             (let* ([mail (get-mail)]
                    [valq (unbox mail)])
               (if (empty-queue? valq)
                   #f
                   (dequeue valq
                            (λ (1st-v other-vs)
                              (set-box! mail other-vs)
                              1st-v))))))

         (define thread-receive
           (λ ()
             (cond [(empty-queue? (unbox (get-mail)))
                    (place-on-ready-queue!
                     (λ () (thread-receive))
                     (get-ptid) (get-tid) (get-mail))
                    (run-next-thread)]
                   [else (thread-try-receive)])))


         (define spawn
           (λ (ctx)
             (let* ([spawn-tid (get-ntid)]
                    [spawn-thk (λ () (ctx spawn-tid))])
               (place-on-ready-queue! spawn-thk (get-tid) spawn-tid (box (empty-queue))))))

         (define yield
           (λ ()
             (let/cc cc
               (spawn (λ (_) (cc (get-tid))))
               (run-next-thread))))


         (define make-mutex
           (λ (keys wait-queue)
             (vector 'mutex keys wait-queue)))

         (define mutex?
           (λ (arg)
             (and (vector? arg)
                  (not (immutable? arg))
                  (eqv? 'mutex (vector-ref arg 0)))))

         (define mutex-keys       (λ (mut) (if (mutex? mut) (vector-ref mut 1) #f)))
         (define mutex-wait-queue (λ (mut) (if (mutex? mut) (vector-ref mut 2) #f)))

         (define set-mutex-keys!       (λ (mut keys) (if (mutex? mut) (vector-set! mut 1 keys) #f)))
         (define set-mutex-wait-queue! (λ (mut waqu) (if (mutex? mut) (vector-set! mut 2 waqu) #f)))

         (define mutex (λ (keys) (make-mutex keys (empty-queue))))


         (define wait
           (λ (mut)
             (let/cc cc
               (let ([thk (λ () (cc (void)))]
                     [keys (mutex-keys mut)]
                     [wait-queue (mutex-wait-queue mut)])
                 (cond [(zero? keys)
                        (set-mutex-wait-queue!
                         mut
                         (place-on-thread-queue
                          wait-queue
                          thk (get-ptid) (get-tid) (get-mail)))
                        (run-next-thread)]
                       [else
                        (set-mutex-keys! mut (sub1 keys))
                        (thk)])))))

         (define signal
           (λ (mut)
             (let/cc cc
               (let ([thk (λ () (cc (void)))]
                     [keys (mutex-keys mut)]
                     [wait-queue (mutex-wait-queue mut)])
                 (if (empty-queue? wait-queue)
                     (set-mutex-keys! mut (add1 keys))
                     (dequeue wait-queue
                              (λ (1st-waiting-tid other-waiting-tids)
                                (set! the-ready-queue (enqueue the-ready-queue 1st-waiting-tid))
                                (set-mutex-wait-queue! mut other-waiting-tids))))))))


         (let ([*apply* apply])
           (define apply
             (λ (func args)
               (let/cc return
                 (cond [(time-expired?)
                        (place-on-ready-queue!
                         (λ () (return (apply func args)))
                         (get-ptid) (get-tid) (get-mail))
                        (run-next-thread)]
                       [else
                        (decrement-timer!)
                        (return (*apply* func args))]))))
           ,code))))

  (define insert-module
    (λ (module-name code)
      (define _ (gensym '_))
      `(begin
         (module ,_ racket
           (require ,module-name)
           ,code)
         (require ',_))))


  (let ()
    (define m
      (insert-module
       "../types/types.rkt"
       code))
    (displayln (format "initial ~a:" i))
    (pretty-print m)
    #;(eval m eval-ns)
    (newline))

  (let ()
    (define m
      (insert-module
       "../types/types.rkt"
       (auto-apply
        (desugar
         code))))
    (displayln (format "auto-apply ~a:" i))
    (pretty-print m)
    #;(eval m eval-ns)
    (newline))

  (let ()
    (define m
      (insert-module
       "../types/types.rkt"
       (insert-dequeue
        (insert-thread
         (module/exit
          (module/thread
           (auto-apply
            (desugar
             code))))))))
    (displayln (format "module/thread ~a:" i))
    (pretty-print m)
    (eval m eval-ns)
    (newline))

  #;(let ()
      (define m
        (insert-module
         "../base/cpsed.rkt"
         (desugar
          (auto-cps
           (desugar
            (insert-dequeue
             (insert-thread
              (module/thread
               (auto-apply
                (desugar
                 code))))))))))
      (displayln (format "auto-cpsed ~a:" i))
      (pretty-print m)
      #;(eval m eval-ns)
      (newline))

  #;(let ()
      (define m
        (insert-module
         "../base/cpsed.rkt"
         (parser
          (desugar
           (auto-cps
            (desugar
             (insert-dequeue
              (insert-thread
               (module/thread
                (auto-apply
                 (desugar
                  code))
                1)))))))))
      (displayln (format "auto-cpsed ~a:" i))
      (pretty-print m)
      #;(eval m eval-ns)
      (newline))

  (newline))
