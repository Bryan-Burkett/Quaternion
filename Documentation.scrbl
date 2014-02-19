#lang scribble/manual
@(require (for-label racket))

@title{Quaternions in Racket:}

@larger{A project for EECS 448 at the University of Kansas}

@centered{@tabular[#:sep @hspace[1]
                     (list (list "Austin Applegate" "Justin Arnspiger" "Evan Bissell" "Bryan Burkett")
                           (list "Miguel Calderon Mejia" "Eric Chanthorabout" "Quin Chen" "Dawson Conway")
                           (list "Jonathan Coup" "Aaron Cowdrey" "Wei Fei" "Thomas Ford"))]}

@section{First Section}
@racketblock[(define example (display "This is an example"))]
@defproc[(make-Quaternion [x (number?)]) quaternion?]{Takes any number and returns it as a Quaternion 
                                                      @codeblock|{
                                                                  (make-quaternion 3)
                                                                  (quaternion 3 0 0 0)
                                                                  (make-quaternion 3+2i)
                                                                  (quaternion 3 2 0 0)}|}

