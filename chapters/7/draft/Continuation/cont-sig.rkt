#lang typed/racket

(require "../types/types.rkt")

(provide cont^)


(define-signature cont^
  (
   [id-cont              : [-> Null]]
   [end-cont             : [-> Null]]
   [end-subthread-cont   : [-> (List Frame)]]
   [end-main-thread-cont : [-> (List Frame)]]

   [apply-cont    : [-> Cont ExpVal FinalAnswer]]
   [apply-handler : [-> Cont DenVal FinalAnswer]]

   [inherit-handlers-cont : [-> Cont (Option Handlers-Cont)]]
   ))
