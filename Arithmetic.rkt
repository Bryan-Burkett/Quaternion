#lang racket
 
(struct quaternion (h i j k))
 
;(define (add . quaternions)
;  "add quaternions")

; I just made everything a quaternion before adding it
(define (make-quaternion x)
  (if (quaternion? x)
      x
      (if (number? x)
          (if (complex? x)
              (quaternion (real-part x) (imag-part x) 0 0)
              (quaternion x 0 0 0))
          (quaternion 0 0 0 0))))

(define (add . quaternions)
  (let ((quaternion-list (map make-quaternion quaternions)))
    (quaternion (apply + (map quaternion-h quaternion-list))
                (apply + (map quaternion-i quaternion-list))
                (apply + (map quaternion-j quaternion-list))
                (apply + (map quaternion-k quaternion-list)))))
 
(define (subtract . quaternions)
  "subtract quaternions")

(define (multiply . quaternions)
  "multiply quaternions")

(define (divide . quaternions)
  "divide quaternions")
 
(define (conjugate quaternion)
  "conjugate quaternion")

(define (magnitude q)
  "magnitude of q")

(define (exp q)
  "e^q")

(define (log q)
  "logarithm of q")

(define (sin q)
  "sin of q")

(define (cos q)
  "cos of q")

(define (expt quaterion1 quaternion2)
  "quaternion1 ^ quaternion2")

(define (quaternion-equal quaternion1 quaternion2)
  "are q1 and q2 equal")
  

; Hopefully we can do something to the struct instead of using this

(define (quaternion->list quaternion)
  (list (quaternion-h quaternion)
        (quaternion-i quaternion)
        (quaternion-j quaternion)
        (quaternion-k quaternion)))

; testing

(define q1 (quaternion 1 2 3 4))
(define q2 (quaternion 2 3 4 5))
(define q3 (quaternion 3 4 5 6))
(define q-sum (add q1 q2 q3))
(quaternion->list q-sum)
(quaternion->list (add 1 4+4i (quaternion 1 2 3 4)))