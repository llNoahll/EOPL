#lang typed/racket

(require "../types/types.rkt")

(provide cont^)


(define-signature cont^
  (
   [id-cont    : [-> Cont]]
   [end-cont   : [-> Cont]]
   [apply-cont : [-> Cont ExpVal FinalAnswer]]
   ))
