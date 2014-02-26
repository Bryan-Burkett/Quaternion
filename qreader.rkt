;module for storing the parser and reader functions
#lang racket

;----------------------------------------------------------
(define (GoodExpression? s) (not (eq? #f(andmap myRegexMatch (myRegExpSplit s)))))  ;Is the expression acceptable?
   (define (myRegExpSplit s) (regexp-split #rx"[-+]" s))              ;Splits a string by "+" and "-" operaters
   (define (myRegexMatch s) (regexp-match termRegex s))               ;Does s match termRegex
   (define termRegex #px"^(([0-9]*[i|j|k])|([0-9]))$")                ;regex for a single term in expression
;----------------------------------------------------------
