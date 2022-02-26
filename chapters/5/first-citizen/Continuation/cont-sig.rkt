#lang typed/racket

(require "../types/types.rkt")

(provide cont^)


(define-signature cont^
  (
   [id-cont*              : [-> Id-Cont*]]
   [end-cont*             : [-> End-Cont*]]
   [end-subthread-cont*   : [-> End-Cont*]]
   [end-main-thread-cont* : [-> End-Cont*]]

   [apply-cont    : [-> Cont  ExpVal FinalAnswer]]
   [apply-handler : [-> Cont* DenVal FinalAnswer]]

   [inherit-handlers-cont* : [-> Cont* (Option Handlers-Cont*)]]
   ))
