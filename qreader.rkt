;module for storing the parser and reader functions
#lang racket
(require "Quaternion.rkt")
;----------------------------------------------------------
;Checks if an expression is in the correct syntax to for the quaternion conversion
(define (GoodExpression? s) (not (eq? #f(andmap myRegexMatch (myRegExpSplit s)))))                             ;Is the expression acceptable?
   (define (myRegExpSplit s) 
     (if (equal? (substring s 0 1) "-") (regexp-split #rx"[-+]" (substring s 1)) (regexp-split #rx"[-+]" s)))  ;Splits a string by "+" and "-" operaters
   (define (myRegexMatch s) (regexp-match termRegex s))                                                        ;Does s match termRegex
   (define termRegex #px"^(((([0-9]*([.][0-9]*)?)|([0-9]+[/][0-9]+))[i|j|k])|([0-9]+)|([0-9]+[/][0-9]+))$")    ;regex for a single term in expression
;----------------------------------------------------------

;Turns any expression into a quaternion, in which each term is connected by a + or -, and the term is scalar or ends in i j k
;example, 8.8j+9/3k-2+0i-3i+6/1000 should return (quaternion -1 497/500 -3 8.8 3)

(define (Expression->Quaternion s)
  (if (GoodExpression? s) 
      (let ([myTerms (regexp-match* #rx"(((([0-9]*([.][0-9]*)?)|([0-9]+[/][0-9]+))[i|j|k])|([0-9]+[/][0-9]+)|([0-9]+))|[+-]" s)])
        ;(define-values (myh myi myj myk) (values 0 0 0 0));;;my addition
        (define (addTerm terms h i j k)
          (if (empty? terms) (quaternion h i j k)
              {let ()
               ;(print "  h:") (print h) (print "  i:") (print i) (print "  j:") (print j) (print "  k:")  (print k) (print "  terms:")  (print terms)
                (define thisTerm (car terms))
                (define nextTerm (cadr terms))
                (define lastCharOfNext (substring nextTerm (- (string-length nextTerm) 1 )))
                (define AllButlastCharOfNext (substring nextTerm 0 (- (string-length nextTerm) 1 )))
                (define sign (if (equal? thisTerm "-") -1 1))
                (define getNonScalar 
                  (if (equal? AllButlastCharOfNext "") 
                      1 ;This is the instance that the term is a standalone i j or k
                      (* sign (string->number AllButlastCharOfNext))))
                (cond
                  [(equal? lastCharOfNext "i") (addTerm (cddr terms) h (+ i getNonScalar) j k)]
                  [(equal? lastCharOfNext "j") (addTerm (cddr terms) h i (+ j getNonScalar)  k)]
                  [(equal? lastCharOfNext "k") (addTerm (cddr terms) h i j (+ k getNonScalar))]
                  [else (addTerm (cddr terms) (+ h (* sign (string->number nextTerm))) i j k)])}))
        (if (equal? (substring s 0 1) "-") 
            (addTerm (append '("+")(append '("0") myTerms)) 0 0 0 0)
            (addTerm (append '("+") myTerms) 0 0 0 0)))
      
      "Syntax Error: expression not valid."))
