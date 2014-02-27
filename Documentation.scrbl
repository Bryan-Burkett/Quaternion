#lang scribble/manual

@(declare-exporting "Quaternion.rkt")


@title{Quaternions in Racket:}


@larger{A project for EECS 448 at the University of Kansas}

@centered{@tabular[#:sep @hspace[1]
                     (list (list "Austin Applegate" "Justin Arnspiger" "Evan Bissell" "Bryan Burkett")
                           (list "Miguel Calderon Mejia" "Eric Chanthorabout" "Quin Chen" "Tim Clark")
                           (list "Dawson Conway" "Jonathan Coup" "Aaron Cowdrey" "Thomas Ford"))]}

@section{Guide}

@subsecion{Making a Quaternion}
Quaternions are kdfja;dfa

To create a quaternion in racket, type
@codeblock|{
            >(quaternion 1 2 3 4)
            (quaternion 1 2 3 4)
            }|
This represents 

@subsection{Quaternion}
@defstruct[quaternion ([h real?] [i real?] [j real?] [k real?])]{A structure to represent a quaternion. @italic{h} represents the real part, @italic{i}, @italic{j} and @italic{k} represent the imaginary parts.}

@defproc[(eq? [x quaternion?/number?][y quaternion?/number?]) quaternion?/number?]{Returns @italic{true} if the two arguments are equevilent mathematically, otherwise returns false. If none of the arguments are quaternions, uses racket's default eq? function.  
                                                      @codeblock|{
                                                                  >(eq? (quaternion 2 5 34 -1) (quaternion 2 5 34 -1))
                                                                  #t
                                                                  >(eq? (quaternion 2 5 34 -1) (quaternion -1 0 4 16))
                                                                  #f}|}
@defproc[(scalar-part [x quaternion?/number?]) number?]{Returns the real part of the input as a real number.
                                                      @codeblock|{ 
                                                                  >(scalar-part (quaternion 2 5 34 -1))
                                                                  2}|}

@defproc[(vector-part [x quaternion?/number?]) quaternion?]{Returns the imaginary part of the input as a quaternion with real part 0.
                                                      @codeblock|{
                                                                  >(vector-part (quaternion 2 5 34 -1))
                                                                  (quaternion 0 5 34 -1)}|}
@defproc[(conjugate [w quaternion?/number?])quaternion?/number?]{Returns the conjugate of @italic{w}. This is equivilent to (- (real-part w) (vector-part w))]. If none of the arguments are quaternions, uses rackets default conjugate procedure.
                                                      @codeblock|{
                                                                  >(conjugate (quaternion 2 5 34 -1))
                                                                  (quaternion  2 -5 -34 1)}|}}

@defproc[(norm [w quaternion?])number?]{@codeblock|{(define (quaternion-norm number)
  (let ((q (make-quaternion number)))
    (sqrt (+ (expt (quaternion-h q) 2)
       (expt (quaternion-i q) 2)
       (expt (quaternion-j q) 2)
       (expt (quaternion-k q) 2)))))}| This take a quaternion as input. It squares each component of the quaternion, sums those values and returns the square root of that sum. }

@defproc[(unit [w quaternion?])number?]{Returns the unit quaternion of @italic{w}. This is the quaternion divided by its norm.}

@subsection{Basic Operators}

@defproc[(+ [x quaternion?/number?]...) quaternion?/number?]{Returns the sum of all @italic{x}s, or for only one @italic{x} returns @italic{x}. If none of the arguments are quaternions, uses racket's default + procedure.
                                                      @codeblock|{
                                                                  (+ (quaternion 2 5 34 -1) (quaternion -1 0 4 16))
                                                                  (quaternion 1 5 38 15)}|}


@defproc[(- [w quaternion?/number?])quaternion?/number?]{Returns the opposite of @italic{w}. The same as (- 0 w)].}
@defproc[(- [x quaternion?/number?][w quaternion?/number?]...)quaternion?/number?]{Returns the difference of @italic{w} from @italic{x}. For multiple @italic{w}s takes the difference of each @italic{w} from @italic{x} working left to right. If none of the @italic{x}s are quaternions, uses racket's default - procedure.
                                                      @codeblock|{
                                                                  >(- (quaternion 2 5 34 -1) (quaternion -1 0 4 16))
                                                                  (quaternion 3 5 30 -17)}|}

@defproc[(* [x quaternion?/number?]...) quaternion?/number?]{Returns the product of all @italic{x}s, or for only one @italic{x} returns @italic{x}. If none of the arguments are quaternions, uses racket's default * procedure.
                                                      @codeblock|{
                                                                  >(* (quaternion 2 5 34 -1) (quaternion -1 0 4 16))
                                                                  (quaternion -122 543 -106 53)}|}


@defproc[(/ [w quaternion?/number?])quaternion?/number?]{Returns the reciprocal of @italic{w}. The same as (/ 1 w)].}
@defproc[(/ [x quaternion?/number?][w quaternion?/number?]...)quaternion?/number?]{Returns the quotient of @italic{x} over @italic{w}. For multiple @italic{w}s takes the quotient of each @italic{w} from @italic{x} working left to right. If any @italic{w} is exact 0, the exn:fail:contract:divide-by-zero] exn:fail:contract:divide-by-zero exception is raised. If none of the arguments are quaternions, uses racket's default / procedure.
                                                      @codeblock|{
                                                                  >(- (quaternion 2 5 34 -1) (quaternion -1 0 4 16))
                                                                  (quaternion 3 5 30 -17)}|}

@subsection{Advanced Operators}


@defproc[(exp [x quaternion?/number?]) quaternion?/number?]{Returns the Euler's number to the power of @italic{x}. If @italic{x} is not a quaternion, uses racket's default exp procedure.
                                                      @codeblock|{
                                                                  >(+ (quaternion 2 5 34 -1) (quaternion -1 0 4 16))
                                                                  (quaternion 1 5 38 15)}|}
@defproc[(expt [x quaternion?/number?][y quaternion?/number?]) quaternion?/number?]{Returns the @italic{x} to the power of @italic{y}. If if neither argument is a quaternion, uses racket's default expt procedure.
                                                      @codeblock|{
                                                                  >(+ (quaternion 2 5 34 -1) (quaternion -1 0 4 16))
                                                                  (quaternion 1 5 38 15)}|}


@defproc[(sin [x quaternion?/number?]) quaternion?/number?]{Returns the sine of @italic{x}. If @italic{x} is not a quaternion, uses racket's default sin procedure.
                                                      @codeblock|{
                                                                  >(+ (quaternion 2 5 34 -1) (quaternion -1 0 4 16))
                                                                  (quaternion 1 5 38 15)}|}


@defproc[(cos [x quaternion?/number?]) quaternion?/number?]{Returns the cosine of @italic{x}. If @italic{x} is not a quaternion, uses racket's default cos procedure.
                                                      @codeblock|{
                                                                  >(+ (quaternion 2 5 34 -1) (quaternion -1 0 4 16))
                                                                  (quaternion 1 5 38 15)}|}


