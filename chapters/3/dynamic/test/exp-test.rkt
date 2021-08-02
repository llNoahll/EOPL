#lang racket

(require "../base/base.rkt")

(value-of (if-exp (bool-exp #t) (const-exp 1) (const-exp 2))
          (base-env))

(value-of (if-exp (bool-exp #f) (const-exp 1) (const-exp 2))
          (base-env))

(value-of (proc-exp
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
          (base-env))
