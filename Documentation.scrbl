#lang scribble/manual
@(require (for-label racket))


@title{Quaternions in Racket:}

@larger{A project for EECS 448 at the University of Kansas}

@centered{@tabular[#:sep @hspace[1]
                     (list (list "Austin Applegate" "Justin Arnspiger" "Evan Bissell" "Bryan Burkett")
                           (list "Miguel Calderon Mejia" "Eric Chanthorabout" "Quin Chen" "Tim Clark")
                           (list "Dawson Conway" "Jonathan Coup" "Aaron Cowdrey" "Thomas Ford"))]}

@section{Quaternion}
@defstruct[quaternion ([h real?] [i real?] [j real?] [k real?])]{A structure to represent a quaternion. @italic{h} represents the real part, @italic{i}, @italic{j} and @italic{5} represent the imaginary parts.}

@defproc[(eq? [x quaternion?/number?][y quaternion?/number?]) quaternion?/number?]{Returns @italic{true} if the two arguments are equevilent mathematically, otherwise returns false. If none of the arguments are quaternions, uses racket's default eq? function. } 
                                                      @codeblock|{
                                                                  >(+ (quaternion 2 5 34 -1) (quaternion -1 0 4 16))
                                                                  (quaternion 1 5 38 15)}|}



@section{Basic Operators}
@subsection{Addition}
@defproc[(+ [x quaternion?/number?]...) quaternion?/number?]{Returns the sum of all @italic{x}s, or for only one @italic{x} returns @italic{x}. If none of the arguments are quaternions, uses racket's default + procedure.
                                                      @codeblock|{
                                                                  >(+ (quaternion 2 5 34 -1) (quaternion -1 0 4 16))
                                                                  (quaternion 1 5 38 15)}|}

@subsection{Subtraction}
@defproc[(- [w quaternion?/number?])quaternion?/number?]{Returns the opposite of @italic{w}. The same as @racket[(- 0 w)].}
@defproc[(- [x quaternion?/number?][w quaternion?/number?]...)quaternion?/number?]{Returns the difference of @italic{w} from @italic{x}. For multiple @italic{w}s takes the difference of each @italic{w} from @italic{x} working left to right. If none of the @italic{x}s are quaternions, uses racket's default - function.
                                                      @codeblock|{
                                                                  >(- (quaternion 2 5 34 -1) (quaternion -1 0 4 16))
                                                                  (quaternion 3 5 30 -17)}|}
@subsection{Multiplication}
@defproc[(* [x quaternion?/number?]...) quaternion?/number?]{Returns the product of all @italic{x}s, or for only one @italic{x} returns @italic{x}. If none of the arguments are quaternions, uses racket's default * procedure.
                                                      @codeblock|{
                                                                  >(* (quaternion 2 5 34 -1) (quaternion -1 0 4 16))
                                                                  (quaternion -122 543 -106 53)}|}

@subsection{Division}
@defproc[(/ [w quaternion?/number?])quaternion?/number?]{Returns the reciprocal of @italic{w}. The same as @racket[(/ 1 w)].}
@defproc[(/ [x quaternion?/number?][w quaternion?/number?]...)quaternion?/number?]{Returns the quotient of @italic{x} over @italic{w}. For multiple @italic{w}s takes the quotient of each @italic{w} from @italic{x} working left to right. If any @italic{w} is exact 0, the exn:fail:contract:divide-by-zero exception is raised. If none of the arguments are quaternions, uses racket's default / function.
                                                      @codeblock|{
                                                                  >(- (quaternion 2 5 34 -1) (quaternion -1 0 4 16))
                                                                  (quaternion 3 5 30 -17)}|}

@section{Advanced Operators}

@subsection{Exponentiation}
@defproc[(exp [x quaternion?/number?]) quaternion?/number?]{Returns the e to the power of @italic{x}. If @italic{x} is not a quaternion, uses racket's default exp function.
                                                      @codeblock|{
                                                                  >(+ (quaternion 2 5 34 -1) (quaternion -1 0 4 16))
                                                                  (quaternion 1 5 38 15)}|}
@defproc[(exp [x quaternion?/number?][y quaternion?/number?]) quaternion?/number?]{Returns the @italic{x} to the power of @italic{y}. If if neither argument is a quaternion, uses racket's default expt function.
                                                      @codeblock|{
                                                                  >(+ (quaternion 2 5 34 -1) (quaternion -1 0 4 16))
                                                                  (quaternion 1 5 38 15)}|}

@subsection{Sine}
@defproc[(sin [x quaternion?/number?]) quaternion?/number?]{Returns the sine of @italic{x}. If @italic{x} is not a quaternion, uses racket's default sin function.
                                                      @codeblock|{
                                                                  >(+ (quaternion 2 5 34 -1) (quaternion -1 0 4 16))
                                                                  (quaternion 1 5 38 15)}|}

@subsection{Cosine}
@defproc[(cos [x quaternion?/number?]) quaternion?/number?]{Returns the cosine of @italic{x}. If @italic{x} is not a quaternion, uses racket's default cos function.
                                                      @codeblock|{
                                                                  >(+ (quaternion 2 5 34 -1) (quaternion -1 0 4 16))
                                                                  (quaternion 1 5 38 15)}|}

@subsection{make-quaternion}
@defproc[(make-quaternion [x quaternion/number?]) quaternion?]{Takes any number and returns it as a Quaternion 
                                                      @codeblock|{
                                                                  >(make-quaternion 3)
                                                                  (quaternion 3 0 0 0)
                                                                  >(make-quaternion 3+2i)
                                                                  (quaternion 3 2 0 0)}|}
