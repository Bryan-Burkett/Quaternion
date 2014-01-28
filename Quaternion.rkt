#lang racket
 
(struct quaternion (a b c d)
  #:transparent)
 
(define-match-expander quaternion: 
  (λ (stx)
    (syntax-case stx ()
      [(_ a b c d) 
       #'(or (quaternion a b c d)
             (and a (app (λ(_) 0) b) (app (λ(_) 0) c) (app (λ(_) 0) d)))])))
 
(define (norm q)
  (match q
    [(quaternion: a b c d)
     (sqrt (+ (sqr a) (sqr b) (sqr c) (sqr d)))]))
 
(define (negate q)
  (match q
    [(quaternion: a b c d)
     (quaternion (- a) (- b) (- c) (- d))]))
 
(define (conjugate q)
  (match q
    [(quaternion: a b c d)
     (quaternion a (- b) (- c) (- d))]))
 
(define (add q1 q2 . q-rest)
  (let ((ans (match* (q1 q2)
               [((quaternion: a1 b1 c1 d1) (quaternion: a2 b2 c2 d2))
                (quaternion (+ a1 a2) (+ b1 b2) (+ c1 c2) (+ d1 d2))])))
    (if (empty? q-rest)
        ans
        (apply add (cons ans q-rest)))))
 
(define (multiply q1 q2 . q-rest)
  (let ((ans (match* (q1 q2)
               [((quaternion: a1 b1 c1 d1) (quaternion: a2 b2 c2 d2))
                (quaternion (- (* a1 a2) (* b1 b2) (* c1 c2) (* d1 d2))
                            (+ (* a1 b2) (* b1 a2) (* c1 d2) (- (* d1 c2)))
                            (+ (* a1 c2) (- (* b1 d2)) (* c1 a2) (* d1 b2))
                            (+ (* a1 d2) (* b1 c2) (- (* c1 b2)) (* d1 a2)))])))
    (if (empty? q-rest)
        ans
        (apply multiply (cons ans q-rest)))))
