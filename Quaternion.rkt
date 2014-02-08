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

; Should operations like these follow the naming convention? quaternion-scalar-part seems wordy
; Returns the real part of a quaternion or complex number
; For a quaternion a+bi+cj+dk this returns a
(define (scalar-part number)
  (let ((q (make-quaternion number)))
    (quaternion-h q)))

; Returns the vector part of a number
; For a quaternion a+bi+cj+dk, this returns bi+cj+dk
(define (vector-part number)
  (let ((q (make-quaternion number)))
    (quaternion-subtract q (scalar-part q))))

; Because scalar-part and quaternion-subtract use make-quaternion, this could also be defined
(define (vector-part-alternate number)
  (quaternion-subtract number (scalar-part number)))
; but for consistency I figure the first form is better?

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
; Tested: (quaternion-divide (quaternion 2 4 6 8) 2) returns (quaternion 1 2 3 4)
; (quaternion-divide (quaternion 2 4 6 8) (quaternion 1 2 3 4)) returns (quaternion 2.0 0.0 0.0 -5.551115123125783e-17)

(define (quaternion-reciprocal x)
  (let ([norm2 (expt (quaternion-norm x) 2)])
     (quaternion (/ (quaternion-h x)   norm2)
                (/ (- (quaternion-i x)) norm2)
                (/ (- (quaternion-j x)) norm2)
                (/ (- (quaternion-k x)) norm2))
   ))
; Tested: (quaternion-reciprocal (quaternion 1 2 3 4)) returns (quaternion 0.03333333333333333 -0.06666666666666667 -0.1 -0.13333333333333333)
;     verified by worlfram alpha
 
(define (quaternion-conjugate number)
  (let ((q (make-quaternion number)))
    (quaternion (quaternion-h q)
                (- (quaternion-i q))
                (- (quaternion-j q))
                (- (quaternion-k q)))))

(define (quaternion-norm number)
  (let ((q (make-quaternion number)))
    (sqrt (+ (expt (quaternion-h q) 2)
       (expt (quaternion-i q) 2)
       (expt (quaternion-j q) 2)
       (expt (quaternion-k q) 2)))))

(define (quaternion-unit number)
  (let ((q (make-quaternion number)))
    (quaternion-divide q (quaternion-norm q))))

; Returns e^number, for a real, complex, or quaternion value
; Uses the equation e^a * (cos(||v||) + v/||v|| * sin(||v||)) where a is the scalar part, v is the vector part
; When the number is real, the equation divides by zero because the norm of the vector part is 0, so
; we have to check for that case.
(define (quaternion-exp number)
  (let ((a (scalar-part (make-quaternion number)))
        (v (vector-part (make-quaternion number))))
    (if (real? number)
        (exp number)
        (quaternion-multiply (exp a)
                             (quaternion-add (cos (quaternion-norm v))
                                             (quaternion-multiply (quaternion-divide v (quaternion-norm v))
                                                                  (sin (quaternion-norm v))))))))
; Tested: (quaternion-exp (quaternion 1 2 3 4)) returns (quaternion 1.6939227236832994 -0.7895596245415588 -1.184339436812338 -1.5791192490831176)
; I got the same answer working it out with a calculator using the same equation, but haven't found another way to check it.
; (quaternion-exp 3+4i) returns (quaternion -13.128783081462158 -15.200784463067954 0 0), wolfram alpha gives the same answer

; Returns the natural logarithm (base e) of a number
; Uses function ln(||q||) + v/||v|| * arccos(a/||q||) where a is the scalar part, v is the vector part
(define (quaternion-log number)
  (let ((q (make-quaternion number))
        (a (scalar-part (make-quaternion number)))
        (v (vector-part (make-quaternion number))))
    (if (real? number)
        (log number)
        (quaternion-add (log (quaternion-norm q)) (quaternion-multiply (quaternion-unit v) (acos (/ a (quaternion-norm q))))))))
; Tested:
; (quaternion-log 2) returns 0.6931471805599453
; (quaternion-log 3+4i) returns (quaternion 1.6094379124341003 0.9272952180016123 0 0), verified by wolfram alpha
; (quaternion-log (quaternion 1 2 3 4)) returns (quaternion 1.7005986908310777 0.5151902926640851 0.7727854389961277 1.0303805853281702) not verified

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
