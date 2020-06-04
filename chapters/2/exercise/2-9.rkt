#lang typed/racket

(require "../environment/env-unit-2.rkt")

(require/typed "../environment/env-sig.rkt"
  [#:signature env^
   ([empty-env    : [-> Empty-Env]]
    [empty-env?   : [-> Any Boolean : Empty-Env]]
    [extend-env   : [-> Symbol Any Env Extend-Env]]
    [extend-env*  : [-> (Listof Symbol) (Listof Any) Env Extend-Env]]
    [extend-env?  : [-> Any Boolean]]
    [env?         : [-> Any Boolean]]
    [apply-env    : [-> Env Symbol Any]]
    [has-binding? : [-> Env Symbol Boolean]])])


(define-values/invoke-unit env@
  (import)
  (export env^))


(let* ([env-1 : Empty-Env  (empty-env)]
       [env-2 : Extend-Env (extend-env 'x 1 env-1)]
       [env-3 : Extend-Env (extend-env 'y 2 env-2)]
       [env-4 : Extend-Env (extend-env 'z 3 env-3)]
       [env-5 : Extend-Env (extend-env 'x 0 env-4)])

  (displayln "env-1")
  (displayln env-1)
  (displayln (empty-env? env-1))
  (displayln (extend-env? env-1))
  (displayln (env? env-1))
  (displayln (has-binding? env-1 'x))
  (displayln (has-binding? env-1 'y))
  (displayln (has-binding? env-1 'z))
  (newline)

  (displayln "env-2")
  (displayln env-2)
  (displayln (empty-env? env-2))
  (displayln (extend-env? env-2))
  (displayln (env? env-2))
  (displayln (apply-env env-2 'x))
  (displayln (has-binding? env-2 'x))
  (displayln (has-binding? env-2 'y))
  (displayln (has-binding? env-2 'z))
  (newline)

  (displayln "env-3")
  (displayln env-3)
  (displayln (empty-env? env-3))
  (displayln (extend-env? env-3))
  (displayln (env? env-3))
  (displayln (apply-env env-3 'x))
  (displayln (apply-env env-3 'y))
  (displayln (has-binding? env-3 'x))
  (displayln (has-binding? env-3 'y))
  (displayln (has-binding? env-3 'z))
  (newline)

  (displayln "env-4")
  (displayln env-4)
  (displayln (empty-env? env-4))
  (displayln (extend-env? env-4))
  (displayln (env? env-4))
  (displayln (apply-env env-4 'x))
  (displayln (apply-env env-4 'y))
  (displayln (apply-env env-4 'z))
  (displayln (has-binding? env-4 'x))
  (displayln (has-binding? env-4 'y))
  (displayln (has-binding? env-4 'z))
  (newline)

  (displayln "env-5")
  (displayln env-5)
  (displayln (empty-env? env-5))
  (displayln (extend-env? env-5))
  (displayln (env? env-5))
  (displayln (apply-env env-5 'x))
  (displayln (apply-env env-5 'y))
  (displayln (apply-env env-5 'z))
  (displayln (has-binding? env-5 'x))
  (displayln (has-binding? env-5 'y))
  (displayln (has-binding? env-5 'z))
  (newline))
