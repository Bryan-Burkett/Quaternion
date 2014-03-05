#lang racket

(provide quaternion (rename-out 
                     [quaternion-add +] 
                     [quaternion-subtract -] 
                     [quaternion-multiply *] 
                     [quaternion-divide /] 
                     [quaternion-cos cos] 
                     [quaternion-sin sin] 
                     [quaternion-exp exp]
                     [quaternion-expt expt]
                     [quaternion-equal eq?]
                     [quaternion-log log]
                     [quaternion-norm norm]
                     [quaternion-conjugate conjugate]
                     [quaternion-reciprocal reciprocal]
                     [quaternion-unit unit])
         q
         scalar-part 
         vector-part 
         quaternion->string)



(struct quaternion (h i j k) #:inspector #f
  
  ;This is an error checking mechanism for the structure.  If an invalid input is entered, such as a string in the coefficient
  ;of the quaternion, an error will be thrown.
  #:guard (lambda (h i j k type-wrong-format)
            (unless (andmap real? (list h i j k)) (error type-wrong-format "Invalid input!")) (values h i j k)))

(define (q h i j k) (quaternion h i j k))

; Returns if the numbers are equivalent, even if one is quaternion and one is real or complex
(define (quaternion-equal number1 number2)
  (if (ormap quaternion? (list number1 number1))
      (let ((quaternion1 (make-quaternion number1)) (quaternion2 (make-quaternion number2)))
        (and (= (quaternion-h quaternion1) (quaternion-h quaternion2))
             (= (quaternion-i quaternion1) (quaternion-i quaternion2))
             (= (quaternion-j quaternion1) (quaternion-j quaternion2))
             (= (quaternion-k quaternion1) (quaternion-k quaternion2))))
      (eq? number1 number2)))

;Takes a quaternion and turns it into a string. Needed for qreader.rkt
(define (quaternion->string quat)
  (let ((q (make-quaternion quat)))(string-append 
   "(quaternion " 
   (number->string (quaternion-h q)) " "
   (number->string (quaternion-i q)) " "
   (number->string (quaternion-j q)) " "
   (number->string (quaternion-k q)) ")"
   )))

; This function takes a real, complex, or quaternion number as input, and returns a quaternion with the equivalent value.
; The arithmetic functions use (make-quaternion parameter) so they can handle real and complex paramters
(define (make-quaternion x)
  (if (quaternion? x)
      x
      (if (number? x)
              (quaternion (real-part x) (imag-part x) 0 0)
          (quaternion 0 0 0 0))))

(define (unmake-quaternion x)
  (if (quaternion? x)
      (if (and (quaternion? x) (= (quaternion-j x) (quaternion-k x) 0))
          (if (= (quaternion-i x) 0)
              (quaternion-h x)
              (make-rectangular (quaternion-h x) (quaternion-i x)))
          x)
      x))

; Should operations like these follow the naming convention? quaternion-scalar-part seems wordy
; Returns the real part of a quaternion or complex number 
; Should this be called real part?
; For a quaternion a+bi+cj+dk this returns a
(define (scalar-part number)
  (let ((q (make-quaternion number)))
    (quaternion-h q)))

; Returns the vector part of a number
; For a quaternion a+bi+cj+dk, this returns bi+cj+dk
(define (vector-part number)
  (let ((q (make-quaternion number)))
    (quaternion-subtract q (scalar-part q)))) ;Should this return a vector? a quaternion with a real part 0 is different from a vector

(define (quaternion-norm number)
  (let ((q (make-quaternion number)))
    (sqrt (+ (expt (quaternion-h q) 2)
             (expt (quaternion-i q) 2)
             (expt (quaternion-j q) 2)
             (expt (quaternion-k q) 2)))))

(define (quaternion-conjugate number)
  (if (quaternion? number)
      (let ((q (make-quaternion number)))
         (quaternion (quaternion-h q)
                     (- (quaternion-i q))
                     (- (quaternion-j q))
                     (- (quaternion-k q))))
      (conjugate number)))

(define (quaternion-reciprocal number)
  (let ([norm2 (expt (quaternion-norm number) 2)])
    (quaternion (/ (quaternion-h number)   norm2)
                (/ (- (quaternion-i number)) norm2)
                (/ (- (quaternion-j number)) norm2)
                (/ (- (quaternion-k number)) norm2))
    ))
; Tested: (quaternion-reciprocal (quaternion 1 2 3 4)) returns (quaternion 0.03333333333333333 -0.06666666666666667 -0.1 -0.13333333333333333)
;     verified by worlfram alpha


(define (quaternion-unit number)
  (let ((q (make-quaternion number)))
    (quaternion-divide q (quaternion-norm q))))


; Take any number of quaternions, real numbers, and complex numbers and return the sum as a quaternion struct
(define (quaternion-add . quaternions)
  (if (ormap quaternion? quaternions)
      (let ((quaternion-list (map make-quaternion quaternions)))
        (quaternion (apply + (map quaternion-h quaternion-list))
                    (apply + (map quaternion-i quaternion-list))
                    (apply + (map quaternion-j quaternion-list))
                    (apply + (map quaternion-k quaternion-list))))
      (apply + quaternions)))

; Take any number of quaternions, real numbers, and complex numbers and return the difference as a quaternion struct
(define (quaternion-subtract . quaternions)
  (if (ormap quaternion? quaternions)
      (let ((quaternion-list (map make-quaternion quaternions)))
        (quaternion (apply - (map quaternion-h quaternion-list))
                    (apply - (map quaternion-i quaternion-list))
                    (apply - (map quaternion-j quaternion-list))
                    (apply - (map quaternion-k quaternion-list))))
      (apply - quaternions)))

(define (quaternion-multiply2  x1 x2)
  (quaternion (apply + (list (* (quaternion-h x1) (quaternion-h x2))  (- (* (quaternion-i x1) (quaternion-i x2))) (- (* (quaternion-j x1) (quaternion-j x2))) (- (* (quaternion-k x1) (quaternion-k x2))))) ;h
              (apply + (list (* (quaternion-h x1) (quaternion-i x2))  (*    (quaternion-i x1) (quaternion-h x2))  (*    (quaternion-j x1) (quaternion-k x2))  (- (* (quaternion-k x1) (quaternion-j x2)))));i
              (apply + (list (* (quaternion-h x1) (quaternion-j x2))  (- (* (quaternion-i x1) (quaternion-k x2))) (*    (quaternion-j x1) (quaternion-h x2))  (*    (quaternion-k x1) (quaternion-i x2)))) ;j
              (apply + (list (* (quaternion-h x1) (quaternion-k x2))  (*    (quaternion-i x1) (quaternion-j x2))  (- (* (quaternion-j x1) (quaternion-i x2))) (*    (quaternion-k x1) (quaternion-h x2)))))) ;k

(define (quaternion-multiply . quaternions)
  (if (ormap quaternion? quaternions)
      (foldr quaternion-multiply2 (quaternion 1 0 0 0) (map make-quaternion quaternions))
      (apply * quaternions)))

(define (quaternion-divide . quaternions)
  (if (ormap quaternion? quaternions)
      (if (null? (cdr quaternions))
          (quaternion-reciprocal (make-quaternion (car quaternions)))
          (foldr (lambda (x1 x2) (quaternion-multiply2 x2 (quaternion-reciprocal x1))) (make-quaternion (car quaternions)) (map make-quaternion (cdr quaternions))))
      (apply / quaternions)))

; Returns e^number, for a real, complex, or quaternion value
; Uses the equation e^a * (cos(||v||) + v/||v|| * sin(||v||)) where a is the scalar part, v is the vector part
; When the number is real, the equation divides by zero because the norm of the vector part is 0, so
; we have to check for that case.
(define (quaternion-exp number)
  (if (quaternion? number) 
      (let ((a (scalar-part (make-quaternion number)))
            (v (vector-part (make-quaternion number))))
        (if (quaternion-equal v 0)
            (exp (unmake-quaternion number))
            (quaternion-multiply (exp a)
                                 (quaternion-add (cos (quaternion-norm v))
                                                 (quaternion-multiply (quaternion-unit v)
                                                                      (sin (quaternion-norm v)))))))
      (exp number)))
; Tested: (quaternion-exp (quaternion 1 2 3 4)) returns (quaternion 1.6939227236832994 -0.7895596245415588 -1.184339436812338 -1.5791192490831176)
; I got the same answer working it out with a calculator using the same equation, but haven't found another way to check it.
; (quaternion-exp 3+4i) returns (quaternion -13.128783081462158 -15.200784463067954 0 0), wolfram alpha gives the same answer

; Returns the natural logarithm (base e) of a number
; Uses function ln(||q||) + v/||v|| * arccos(a/||q||) where a is the scalar part, v is the vector part
(define (quaternion-log number)
  (if (quaternion? number) 
      (let ((q (make-quaternion number))
            (a (scalar-part (make-quaternion number)))
            (v (vector-part (make-quaternion number))))
        (if (quaternion-equal v 0)
            (log number)
            (quaternion-add (log (quaternion-norm q)) (quaternion-multiply (quaternion-unit v) (acos (/ a (quaternion-norm q)))))))
      (log number)))
; Tested:
; (quaternion-log 2) returns 0.6931471805599453
; (quaternion-log 3+4i) returns (quaternion 1.6094379124341003 0.9272952180016123 0 0), verified by wolfram alpha
; (quaternion-log (quaternion 1 2 3 4)) returns (quaternion 1.7005986908310777 0.5151902926640851 0.7727854389961277 1.0303805853281702) not verified

;since we alread had (exp) and (log) defined, I just turned x^y into e^(y*log(x))
(define (quaternion-expt root power)
  (if (ormap quaternion? (list root power))
      (let ((r (make-quaternion root))
            (p (make-quaternion power))
            (a (scalar-part (make-quaternion root)))
            (v (vector-part (make-quaternion root))))
        (cond [(quaternion-equal (quaternion 0 0 0 0) p)     (quaternion 1 0 0 0)]; power is the zero quaternion, outputs scalar 1
              [(quaternion-equal (quaternion 0 0 0 0) r)     (quaternion 0 0 0 0)]; root is the zero quaternion, outputs scalar 0
              [ else                                         (quaternion-exp (quaternion-multiply power (quaternion-log root)))]))
      (expt root power)))



; New cos and sin functions using the new equations
(define (quaternion-sin q)
  (if (quaternion? q)
      (if (quaternion-equal (vector-part q) 0)
          (sin (unmake-quaternion q))
          (let ((V (quaternion-unit (vector-part q))))
            (quaternion-divide (quaternion-subtract (quaternion-exp (quaternion-multiply q V))
                                                    (quaternion-exp (quaternion-multiply -1 q V)))
                               (quaternion-multiply 2  V))))
      (sin q)))

(define (quaternion-cos q)
  (if (quaternion? q)
      (if (quaternion-equal (vector-part q) 0)
          (cos (unmake-quaternion q))
          (let ((V (quaternion-unit (vector-part q))))
            (quaternion-divide (quaternion-add (quaternion-exp (quaternion-multiply q V))
                                               (quaternion-exp (quaternion-multiply -1 q V))) 2)))
          (cos (unmake-quaternion q))))