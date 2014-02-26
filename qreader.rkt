;module for storing the parser and reader functions

;----------------------------------------------------------
(define (GoodExpression? s) (not (eq? #f(andmap myRegexMatch (myRegExpSplit s)))))
   (define (myRegExpSplit s) (regexp-split #rx"[-+]" s))
   (define (myRegexMatch s) (regexp-match termRegex s))
   (define termRegex #px"^(([0-9]*[i|j|k])|([0-9]))$")
;----------------------------------------------------------
