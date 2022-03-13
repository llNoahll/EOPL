#lang racket

(require "../base/base.rkt")

(value-of/k (if-exp (bool-exp #t) (real-exp 1) (real-exp 2))
            (base-env)
            (end-cont))

(value-of/k (if-exp (bool-exp #f) (real-exp 1) (real-exp 2))
            (base-env)
            (end-cont))

(value-of/k (proc-exp
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
            (base-env)
            (end-cont))
