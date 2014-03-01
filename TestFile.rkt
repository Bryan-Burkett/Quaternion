#lang racket
(require "Quaternion.rkt")

;It was found that the reader does not function correctly when given just -i,-j, or -k:
;>1-i-k
;(quaternion 1 1 0 1)
;I think that this can be fixed just by replacing a few lines of code with something like this:
;(if (equal? AllButlastCharOfNext "") ;This is the instance that the term is a standalone i j or k
 ;                     (if(equal? sign -1) -1           ;This assigns the minus sign when it is just -i -j or -k
  ;                    1) 
   ;                   (* sign (string->number AllButlastCharOfNext))))
   
   ;This generated an error for some reason:
   ;>.05+.005j
;. . .05+.005j: undefined;
 ;cannot reference an identifier before its definition
