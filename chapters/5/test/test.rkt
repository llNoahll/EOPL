#lang typed/racket

(require "../base/base.rkt")


(value-of (let-exp
           '(x)
           (list (const-exp 10))
           (proc-exp
            '()
            (begin-exp
              (list
               (if-exp
                (call-exp (var-exp 'zero?) (list (var-exp 'x)))
                (const-exp 1)
                (begin-exp
                  (list
                   ;; (assign-exp
                   ;;  'x
                   ;;  (call-exp (var-exp '-) (list (var-exp 'x) (const-exp 1))))
                   (var-exp 'x))))))))
          (base-env)
          (end-cont))

;; (value-of
;;  (let-exp
;;   '(x)
;;   (list (const-exp 0))
;;   (begin-exp
;;     (list
;;      (letrec-exp
;;       '(even odd)
;;       (list
;;        (proc-exp
;;         '()
;;         (begin-exp
;;           (list
;;            (if-exp
;;             (call-exp (var-exp 'zero?) (list (var-exp 'x)))
;;             (const-exp 1)
;;             (begin-exp
;;               (list
;;                (assign-exp
;;                 'x
;;                 (call-exp (var-exp '-) (list (var-exp 'x) (const-exp 1))))
;;                (call-exp (var-exp 'odd) (list))))))))
;;        (proc-exp
;;         '()
;;         (begin-exp
;;           (list
;;            (if-exp
;;             (call-exp (var-exp 'zero?) (list (var-exp 'x)))
;;             (const-exp 0)
;;             (begin-exp
;;               (list
;;                (assign-exp
;;                 'x
;;                 (call-exp (var-exp '-) (list (var-exp 'x) (const-exp 1))))
;;                (call-exp (var-exp 'even) (list)))))))))
;;       (begin-exp
;;         (list
;;          (assign-exp 'x (const-exp 13))
;;          (call-exp (var-exp 'odd) (list))))))))
;;  (base-env)
;;  (end-cont))
