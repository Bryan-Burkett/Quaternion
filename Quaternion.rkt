#lang racket
 
(struct quaternion (h i j k) #:inspector #f)

; This function takes a real, complex, or quaternion number as input, and returns a quaternion with the equivalent value.
; The arithmetic functions use (make-quaternion parameter) so they can handle real and complex paramters
(define (make-quaternion x)
  (if (quaternion? x)
      x
      (if (number? x)
          (if (complex? x)
              (quaternion (real-part x) (imag-part x) 0 0)
              (quaternion x 0 0 0))
          (quaternion 0 0 0 0))))

(define (unmake-quaternion x)
  (if (and (quaternion? x) (= (quaternion-j x) (quaternion-k x) 0))
           (if (= (quaternion-i x) 0)
               (quaternion-h x)
               (make-rectangular (quaternion-h x) (quaternion-i x)))
           x))

; Take any number of quaternions, real numbers, and complex numbers and return the sum as a quaternion struct
(define (quaternion-add . quaternions)
  (let ((quaternion-list (map make-quaternion quaternions)))
    (quaternion (apply + (map quaternion-h quaternion-list))
                (apply + (map quaternion-i quaternion-list))
                (apply + (map quaternion-j quaternion-list))
                (apply + (map quaternion-k quaternion-list)))))
 
; Take any number of quaternions, real numbers, and complex numbers and return the sum as a quaternion struct
(define (quaternion-subtract . quaternions)
  (let ((quaternion-list (map make-quaternion quaternions)))
    (quaternion (apply - (map quaternion-h quaternion-list))
                (apply - (map quaternion-i quaternion-list))
                (apply - (map quaternion-j quaternion-list))
                (apply - (map quaternion-k quaternion-list)))))

(define (seqOperater x y quaternions)
  (let((q-list (map make-quaternion quaternions)))
    (if (null? (cddr q-list)) ;check to see if it is only 2 items
      (y (car q-list) (cadr q-list));if so, apply y (the function for two) to the 2 items
       (y (apply y (reverse (cdr (reverse q-list)))) (car (reverse q-list))) ;if not, apply x (function for many) to all but last item, and then y (function for 2) them together
       )))

; Not fully tested yet
(define (quaternion-multiply . quaternions)
  (define (quaternion-multiply2  x1 x2)
    (quaternion (apply + (list (* (quaternion-h x1) (quaternion-h x2))  (- (* (quaternion-i x1) (quaternion-i x2))) (- (* (quaternion-j x1) (quaternion-j x2))) (- (* (quaternion-k x1) (quaternion-k x2))))) ;h
                (apply + (list (* (quaternion-h x1) (quaternion-i x2))  (*    (quaternion-i x1) (quaternion-h x2))  (*    (quaternion-j x1) (quaternion-k x2))  (- (* (quaternion-k x1) (quaternion-j x2)))));i
                (apply + (list (* (quaternion-h x1) (quaternion-j x2))  (- (* (quaternion-i x1) (quaternion-k x2))) (*    (quaternion-j x1) (quaternion-h x2))  (*    (quaternion-k x1) (quaternion-i x2)))) ;j
                (apply + (list (* (quaternion-h x1) (quaternion-k x2))  (*    (quaternion-i x1) (quaternion-j x2))  (- (* (quaternion-j x1) (quaternion-i x2))) (*    (quaternion-k x1) (quaternion-h x2)))))) ;k
  (seqOperater quaternion-multiply quaternion-multiply2 quaternions))
  

(define (quaternion-divide . quaternions) 
  (define (quaternion-divide2 x1 x2)
    (quaternion-multiply x1 (quaternion-reciprocal x2)))
  (seqOperater quaternion-divide quaternion-divide2 quaternions))


(define (quaternion-reciprocal x)
  (let ([norm2 (expt (quaternion-norm x) 2)])
     (quaternion (/ (quaternion-h x)   norm2)
                (/ (- (quaternion-i x) norm2))
                (/ (- (quaternion-j x) norm2))
                (/ (- (quaternion-k x) norm2)))
   ))
 
(define (quaternion-conjugate number)
  (let ((q (make-quaternion number)))
    (quaternion (quaternion-h q)
                (- (quaternion-i q))
                (- (quaternion-j q))
                (- (quaternion-k q)))))

; It initially looked like the magnitude was the norm^2, but wikipedia and
; wolfram alpha and math major friend disagree.

(define (quaternion-norm number)
  (let ((q (make-quaternion number)))
    (sqrt (+ (expt (quaternion-h q) 2)
       (expt (quaternion-i q) 2)
       (expt (quaternion-j q) 2)
       (expt (quaternion-k q) 2)))))

(define (quaternion-exp q)
  "e^q")

(define (quaternion-log q)
  "logarithm of q")

(define (quaternion-sin q)
  "sin of q")

(define (quaternion-cos q)
  "cos of q")

(define (quaternion-expt quaterion1 quaternion2)
  "quaternion1 ^ quaternion2")

; Returns if the numbers are equivalent, even if one is quaternion and one is real or complex
(define (quaternion-equal number1 number2)
  (let ((quaternion1 (make-quaternion number1)) (quaternion2 (make-quaternion number2)))
  (and (= (quaternion-h quaternion1) (quaternion-h quaternion2))
       (= (quaternion-i quaternion1) (quaternion-i quaternion2))
       (= (quaternion-j quaternion1) (quaternion-j quaternion2))
       (= (quaternion-k quaternion1) (quaternion-k quaternion2)))))

; testing

(define q1 (quaternion 5 4 3 2))
(define q2 (quaternion 4 5 6 7))
(define q3 (quaternion 3 4 5 6))
(quaternion-add q1 q2 q3)
(quaternion-multiply q1 q2)
