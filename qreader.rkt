;module for storing the parser and reader functions


#lang racket
(require "Quaternion.rkt")
(provide start)
;----------------------------------------------------------
;Checks if an expression is in the correct syntax to for the quaternion conversion
(define (GoodExpression? s)
  (if (equal? "" s) #f 
  (not (eq? #f(andmap myRegexMatch (myRegExpSplit s))))))                             ;Is the expression acceptable?
(define (myRegExpSplit s) 
  (if (equal? (substring s 0 1) "-") (regexp-split #rx"[-+]" (substring s 1)) (regexp-split #rx"[-+]" s)))  ;Splits a string by "+" and "-" operaters
(define (myRegexMatch s) (regexp-match termRegex s))                                                        ;Does s match termRegex
(define termRegex #px"^(([0-9]*([.][0-9]*))|((([0-9]*([.][0-9]*)?)|([0-9]+[/][0-9]+))[i|j|k])|([0-9]+)|([0-9]+[/][0-9]+))$")    ;regex for a single term in expression
;----------------------------------------------------------

;Turns any expression into a quaternion, in which each term is connected by a + or -, and the term is scalar or ends in i j k
;example, 8.8j+9/3k-2+0i-3i+6/1000 should return (quaternion -1 497/500 -3 8.8 3)

(define (Expression->Quaternion s)
  (if (GoodExpression? s) 
      (let ([myTerms (regexp-match* #rx"(((([0-9]*([.][0-9]*)?)|([0-9]+[/][0-9]+))[i|j|k])|([0-9]*([.][0-9]+))|([0-9]+[/][0-9]+)|([0-9]+))|[+-]" s)])
        (define (addTerm terms h i j k)
          (if (empty? terms) (quaternion h i j k)
              {let ()
                ;(print terms)
                (define thisTerm (car terms))
                (define nextTerm (cadr terms))
                (define lastCharOfNext (substring nextTerm (- (string-length nextTerm) 1 )))
                (define AllButlastCharOfNext 
                  (if (equal? (substring nextTerm 0 (- (string-length nextTerm) 1 )) ".")
                      nextTerm
                       (substring nextTerm 0 (- (string-length nextTerm) 1 ))))
                (define sign (if (equal? thisTerm "-") -1 1))
                ;(print AllButlastCharOfNext)
                (define getNonScalar 
                  (if (equal? AllButlastCharOfNext "") 
                      sign ;This is the instance that the term is a standalone i j or k
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

;----------------------------------------------------------
;symbol to string method that handles braces.
(define (mySymbol->string s )
  (if (symbol? s) (symbol->string s)
      
      (if (list? (car s))
          (string-append "(" (mySymbol->string (car s)) ")")
          (if (number? (car s))
              (number->string (car s))
                             (string-join (for/list ([i s])
                                               (cond
                                                 [(list? i) (string-append "(" (mySymbol->string i) ")")]
                                                 [(number? i) (number->string i)]
                                                 [else    (symbol->string i)])
                                               ) " ")
              )
          )
      )
  )
;----------------------------------------------------------
;splits string into the braces, spaces, and ids/numbers
(define (splitToTokens s)
  (regexp-match* #rx"[() ]|([^() ])*" s)
  )

;replaced token with quaternion if it meets the quaternion regex and is not already a number
(define (replaceQuatToken Token)
  (if (GoodExpression? Token) 
      (if (string->number Token) 
         Token 
          (quaternion->string (Expression->Quaternion Token)))
       Token
      )
  )
  
;replaces instances in string with quaternion expressions ("3+i+j+k") with the definition ("(quaterion 3 1 1 1)")
(define (replaceQuatString expString)
  (string-join (map replaceQuatToken (splitToTokens expString)) ""))
  
;Starts the input box loop
(define (start)
  (printf ">")
  (flush-output)
  (let* ([line (read-line)]
         [input (if (eof-object? line)
                    '(quit)
                    (let ([port (open-input-string line)])
                      (for/list ([v (in-port read port)]) v)))])
   
       (print (eval (read (open-input-string (replaceQuatString(mySymbol->string input))))))
    (printf "\n")
   (start)))

;(start) <<< you would think this would work, but it just breaks it completely. I tried it with a time delay as well.
;you MUST use (start) in the prompt.
