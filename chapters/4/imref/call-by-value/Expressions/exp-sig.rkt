#lang typed/racket

(require "../types/types.rkt")

(provide exp^)


(define-signature exp^
  (
   [value-of : [-> Exp Env ExpVal]]
   ))
