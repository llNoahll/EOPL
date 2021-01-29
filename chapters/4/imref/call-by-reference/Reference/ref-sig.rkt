#lang typed/racket

(require "../types/types.rkt")

(provide ref^)


(define-signature ref^
  (
   [newref  : [-> DenVal Ref]]
   [deref   : [-> Ref DenVal]]
   [setref! : [-> Ref DenVal Void]]
   [ref?    : [-> Any Boolean : Ref]]
   ))
