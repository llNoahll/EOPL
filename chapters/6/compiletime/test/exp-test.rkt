#lang racket

(require "../base/base.rkt")


(displayln "Start exp test.\n")

(for ([exp
       (in-list
        (list
         (call-exp
          (call-exp (var-exp 'apply) (list (proc-exp '(v0-75167) (var-exp 'v0-75167))))
          (list
           (proc-exp
            '(k-74046)
            (proc-exp
             '()
             (call-exp
              (call-exp (var-exp 'not) (list (var-exp 'k-74046)))
              (list (bool-exp #t)))))
           (var-exp 'null)))

         (if-exp (bool-exp #t) (real-exp 1) (real-exp 2))

         (if-exp (bool-exp #f) (real-exp 1) (real-exp 2))

         (proc-exp
          'funcs
          (begin-exp
            (list
             (call-exp
              (proc-exp
               '(recur-funcs)
               (begin-exp
                 (list
                  (call-exp (var-exp 'displayln) (list (var-exp 'recur-funcs)))
                  (call-exp
                   (var-exp 'displayln)
                   (list
                    (call-exp (var-exp 'recur-funcs) (list (var-exp 'recur-funcs)))))
                  (call-exp (var-exp 'recur-funcs) (list (var-exp 'recur-funcs))))))
              (list
               (proc-exp
                '(recur-funcs)
                (begin-exp
                  (list
                   (call-exp
                    (var-exp 'map)
                    (list
                     (proc-exp
                      '(func)
                      (begin-exp
                        (list
                         (proc-exp
                          'args
                          (begin-exp
                            (list
                             (call-exp
                              (var-exp 'apply)
                              (list
                               (call-exp
                                (var-exp 'apply)
                                (list
                                 (var-exp 'func)
                                 (call-exp
                                  (var-exp 'recur-funcs)
                                  (list (var-exp 'recur-funcs)))))
                               (var-exp 'args)))))))))
                     (var-exp 'funcs)))))))))))
         ))])
  (pretty-print (value-of/k exp (base-env) (end-cont)))
  (newline))
