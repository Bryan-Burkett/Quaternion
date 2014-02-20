#lang scribble/manual
@(require (for-label racket))


@title{Quaternions in Racket:}

@larger{A project for EECS 448 at the University of Kansas}

@centered{@tabular[#:sep @hspace[1]
                     (list (list "Austin Applegate" "Justin Arnspiger" "Evan Bissell" "Bryan Burkett")
                           (list "Miguel Calderon Mejia" "Eric Chanthorabout" "Quin Chen" "Tim Clark")
                           (list "Dawson Conway" "Jonathan Coup" "Aaron Cowdrey" "Thomas Ford"))]}

@section{Quaternion}
@defstruct[quaternion ([h number?] [i number?] [j number?] [k number?])]{A structure to represent a quaternion. h represents the real part, i, j and k represent the imaginary parts.}

@section{Basic Operators}
@subsection{Addition}
@defproc[(+ [x quaternion?/number?]...) quaternion?/number?]{Returns the sum of two or more numbers or quaternions by summing their components, or for only one argument returns that argument. If none of the arguments are quaternions, uses racket's + procedure.
                                                      @codeblock|{
                                                                  >(+ (quaternion 2 5 34 -1) (quaternion -1 0 4 16))
                                                                  (quaternion 1 5 38 15)}|}

@subsection{Subtraction}
@defproc[(- [x quaternion?/number?]...) quaternion?/number?]{Takes the first argument, then takes the difference of all the following arguments from the first arguemnt, or for only one argument returns that argument. If none of the arguments are quaternions, uses racket's - procedure.
                                                      @codeblock|{
                                                                  >(- (quaternion 2 5 34 -1) (quaternion -1 0 4 16))
                                                                  (quaternion 3 5 30 -17)}|}
@subsection{Multiplication}
@defproc[(* [x quaternion?/number?]...) quaternion?/number?]{Returns the product two or more quaternions or numbers, or for only one argument returns that argument. If none of the arguments are quaternions, uses racket's * procedure.
                                                      @codeblock|{
                                                                  >(* (quaternion 2 5 34 -1) (quaternion -1 0 4 16))
                                                                  (quaternion -122 543 -106 53)}|}

@subsection{Subtraction}
@defproc[(- [x quaternion?/number?]...) quaternion?/number?]{Takes the first argument, then multiplies it by the multaplicative inverse of the remaining arguments, or for only one argument returns that argument. If none of the arguments are quaternions, uses racket's - procedure.
                                                      @codeblock|{
                                                                  > (\ (quaternion -122 543 -106 53) (quaternion -1 0 4 16))
                                                                  (quaternion 2 5 34 -1)}|}

@defproc[(make-Quaternion [x quaternion/number?]) quaternion?]{Takes any number and returns it as a Quaternion 
                                                      @codeblock|{
                                                                  >(make-quaternion 3)
                                                                  (quaternion 3 0 0 0)
                                                                  >(make-quaternion 3+2i)
                                                                  (quaternion 3 2 0 0)}|}
