#lang typed/racket

(require "../types/types.rkt")

(provide cont^)


(define-signature cont^
  (
   [id-cont    : [-> Null]]
   [end-cont   : [-> Null]]

   [apply-cont : [-> Cont ExpVal FinalAnswer]]
   ))
