#lang typed/racket

(require "../base/base.rkt")


(let* ([env-1 : Env (empty-env)]
       [env-2 : Env (extend-env 'x 1 env-1)]
       [env-3 : Env (extend-env* '(a b c d e x) '(0 1 2 3 4 -1) env-2)])

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
  (displayln (apply-env env-3 'a))
  (displayln (apply-env env-3 'b))
  (displayln (apply-env env-3 'c))
  (displayln (has-binding? env-3 'x))
  (displayln (has-binding? env-3 'y))
  (displayln (has-binding? env-3 'z))
  (displayln (has-binding? env-3 'a))
  (displayln (has-binding? env-3 'b))
  (displayln (has-binding? env-3 'c))
  (displayln (has-binding? env-3 'd))
  (displayln (has-binding? env-3 'e))
  (newline))
