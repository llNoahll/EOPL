#lang typed/racket

(require "../types/types.rkt")

(provide (all-defined-out))


(: auto-cps [-> S-Exp S-Exp])
(define auto-cps
  (λ (code)
    (match code
      [`(ann ,v ,t)
       #:when (and (s-exp? v) (type? t))
       ]

      [`(quote ,(? symbol? symbol)) ]
      [`(quote ,(? boolean? bool))  ]
      [`(quote ,(? real? num))      ]
      [`(quote ,(? string? str))    ]
      [`(quote ,(? char? ch))       ]
      [`(quote ,(? s-list? ls))     ]

      [(? boolean? bool) ]
      [(? real? num)     ]
      [(? string? str)   ]
      [(? char? ch)      ]

      [(? symbol? var)   ]

      [`(set! ,var ,exp)
       #:when (and (symbol? var) (s-exp? exp))
       ]

      [`(begin ,exp ,exps ..1)
       #:when (and (s-exp? exp)
                   ((listof? s-exp?) exps))
       ]

      [`(if ,(? s-exp? pred-exp)
            ,(? s-exp? true-exp)
            ,(? s-exp? false-exp))
       ]

      [`(with-handlers ([,pred-exps ,handler-exps] ...)
          ,body-exp)
       #:when (and ((listof? s-exp?) pred-exps)
                   ((listof? s-exp?) handler-exps)
                   (s-exp? body-exp))
       ]
      [`(raise       ,(? s-exp? exp)) ]
      [`(spawn       ,(? s-exp? exp)) ]
      [`(mutex       ,(? s-exp? exp)) ]
      [`(wait        ,(? s-exp? exp)) ]
      [`(signal      ,(? s-exp? exp)) ]
      [`(kill-thread ,(? s-exp? exp)) ]

      [`(thread-send ,(? s-exp? tid-exp) ,(? s-exp? value-exp))
       ]
      ['(thread-receive)     ]
      ['(thread-try-receive) ]
      ['(yield)              ]


      [`(letrec ([,bind-vars ,bind-exps] ...) ,body-exp)
       #:when (and ((listof? symbol?) bind-vars)
                   ((listof? s-exp?)  bind-exps)
                   (s-exp? body-exp))
       ]

      [`(let/cc ,cc-var ,body-exp)
       #:when (and (symbol? cc-var) (s-exp? body-exp))
       ]


      [`(,(? λ?) ,args ,body-exp)
       #:when (and ((or/c symbol? (listof? symbol?)) args)
                   (s-exp? body-exp))
       ]
      [`(,(? trace-λ?) ,args ,body-exp)
       #:when (and ((or/c symbol? (listof? symbol?)) args)
                   (s-exp? body-exp))
       ]

      [`(,op ,exps ...)
       #:when (and (s-exp? op) ((listof? s-exp?) exps))
       ]

      )))
