#lang typed/racket

(require "../types/types.rkt")

(provide thd^)


(define-signature thd^
  (
   [initialize-thread-identifier! : [-> Void]]
   [update-thread-identifier!     : [-> Thd Void]]

   [get-ptid : [-> Natural]]
   [get-tid  : [-> Natural]]
   [get-nid  : [-> Natural]]

   [apply-thd : [-> Thd FinalAnswer]]
   ))
