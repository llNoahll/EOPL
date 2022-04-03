#lang typed/racket

(require "../types/types.rkt")

(provide values^)


(define-signature values^
  (
   [symbol-val         : [-> Symbol DenVal]]
   [num-val            : [-> Real DenVal]]
   [bool-val           : [-> Boolean DenVal]]
   [char-val           : [-> Char DenVal]]
   [string-val         : [-> String DenVal]]
   [bytes-val          : [-> Bytes DenVal]]
   [proc-val           : [-> Proc DenVal]]
   [primitive-proc-val : [-> Primitive-Proc DenVal]]
   [queue-val          : [-> (Queueof DenVal) DenVal]]
   [box-val            : [-> (Boxof DenVal) DenVal]]
   [pair-val           : [-> (Pairof DenVal DenVal) DenVal]]
   [list-val           : [-> (Listof DenVal) DenVal]]
   [vector-val         : [-> (Vectorof DenVal) DenVal]]
   [hash-val           : [-> (HashTable DenVal DenVal) DenVal]]

   [expval->symbol         : [-> ExpVal Symbol]]
   [expval->num            : [-> ExpVal Real]]
   [expval->bool           : [-> ExpVal Boolean]]
   [expval->char           : [-> ExpVal Char]]
   [expval->string         : [-> ExpVal String]]
   [expval->bytes          : [-> ExpVal Bytes]]
   [expval->proc           : [-> ExpVal Proc]]
   [expval->primitive-proc : [-> ExpVal Primitive-Proc]]
   [expval->queue          : [-> ExpVal (Queueof DenVal)]]
   [expval->box            : [-> ExpVal (Boxof DenVal)]]
   [expval->pair           : [-> ExpVal (Pairof DenVal DenVal)]]
   [expval->list           : [-> ExpVal (Listof DenVal)]]
   [expval->vector         : [-> ExpVal (Vectorof DenVal)]]
   [expval->hash           : [-> ExpVal (HashTable DenVal DenVal)]]

   [expval->denval   : [-> ExpVal DenVal]]
   [s-expval->denval : [-> Any DenVal]]
   [s-expval->expval : [-> Any ExpVal]]
   ))
