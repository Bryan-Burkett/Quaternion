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

; THIS FUNCTION CURRENTLY RETURNS THE WRONG ANSWER - GO WITH AARON'S
(define (quaternion-multiply . quaternions)
  (define (multiply-two-quaternions quaternion1 quaternion2)
    (quaternion (- (* (quaternion-h quaternion1) (quaternion-h quaternion2))
                   (* (quaternion-i quaternion1) (quaternion-i quaternion2))
                   (* (quaternion-j quaternion1) (quaternion-j quaternion2))
                   (* (quaternion-k quaternion1) (quaternion-k quaternion2)))
                (+ (* (quaternion-h quaternion1) (quaternion-i quaternion2))
                   (* (quaternion-i quaternion1) (quaternion-h quaternion2))
                   (* (quaternion-j quaternion1) (quaternion-k quaternion2))
                   (- (* (quaternion-k quaternion1) (quaternion-j quaternion2))))
                (+ (* (quaternion-h quaternion1) (quaternion-j quaternion2))
                   (* (quaternion-j quaternion1) (quaternion-h quaternion2))
                   (* (quaternion-k quaternion1) (quaternion-i quaternion2))
                   (- (* (quaternion-i quaternion1) (quaternion-k quaternion2))))
                (+ (* (quaternion-h quaternion1) (quaternion-k quaternion2))
                   (* (quaternion-k quaternion1) (quaternion-h quaternion2))
                   (* (quaternion-i quaternion1) (quaternion-j quaternion2))
                   (- (* (quaternion-j quaternion1) (quaternion-i quaternion2))))))
  (foldl multiply-two-quaternions (car quaternions) (cdr quaternions)))

(define (quaternion-divide . quaternions)
  "divide quaternions")
 
(define (quaternion-conjugate number)
  (let ((q (make-quaternion number)))
    (quaternion (quaternion-h q)
                (- (quaternion-i q))
                (- (quaternion-j q))
                (- (quaternion-k q)))))

(define (quaternion-norm number)
  (let ((q (make-quaternion number)))
    (+ (expt (quaternion-h q) 2)
       (expt (quaternion-i q) 2)
       (expt (quaternion-j q) 2)
       (expt (quaternion-k q) 2))))

(define (quaternion-magnitude number)
  (let ((q (make-quaternion number)))
    (sqrt (quaternion-norm q))))

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

(define (quaternion-equal quaternion1 quaternion2)
  "are q1 and q2 equal")

; testing

(define q1 (quaternion 5 4 3 2))
(define q2 (quaternion 4 5 6 7))
(define q3 (quaternion 3 4 5 6))
(quaternion-add q1 q2 q3)
(quaternion-multiply q1 q2)
