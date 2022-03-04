#lang typed/racket

(require "../types/types.rkt")

(provide thd^)


(define-signature thd^
  (
   [initialize-thread-identifier! : [-> Void]]
   [update-thread-identifier!     : [-> Thd Void]]

   [kill-thread! : [-> Natural (U Void Boolean)]]

   [get-mail : [-> (Boxof (Queueof DenVal))]]
   [get-ptid : [-> Natural]]
   [get-tid  : [-> Natural]]
   [get-nid  : [-> Natural]]

   [has-thread? : [-> Natural Boolean]]
   [get-thread  : [-> Natural (U Boolean Thd)]]
   [add-thread! : [-> Natural Thd Void]]

   [apply-thd : [-> Thd FinalAnswer]]
   ))
