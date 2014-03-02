#lang scribble/manual

@(declare-exporting "Quaternion.rkt")


@title{Quaternions in Racket:}


@larger{A project for EECS 448 at the University of Kansas}

@centered{@tabular[#:sep @hspace[1]
                         (list (list "Austin Applegate" "Justin Arnspiger" "Evan Bissell" "Bryan Burkett")
                               (list "Miguel Calderon Mejia" "Eric Chanthorabout" "Quin Chen" "Tim Clark")
                               (list "Dawson Conway" "Jonathan Coup" "Aaron Cowdrey" "Thomas Ford"))]}

@section{Guide}

@subsection{Making a Quaternion}
Quaternions are kdfja;dfa

To create a quaternion in racket, type
@codeblock|{
            >(quaternion 1 2 3 4)
            (quaternion 1 2 3 4)
            }|
This represents 



@;*****************************************************************************************************************

@section{Reference}

@subsection{Quaternion}
@defstruct[quaternion ([h real?] [i real?] [j real?] [k real?])]{A structure to represent a quaternion. @italic{h} represents the real part, @italic{i}, @italic{j} and @italic{k} represent the imaginary parts. If not provided with 4 real numbers, return a type-wrong-format error. Automatically defines @code{(quaternion? x)} to test if @italic{x}
                                                                                                        is a quaternion. @code{(quaternion-h x)}, @code{(quaternion-i x)}, @code{(quaternion-j x)} and @code{(quaternion-k x)} can be used to get the components of a quaternion @italic{x}.}


@defproc[(eq? [x quaternion?/number?][y quaternion?/number?]) quaternion?/number?]{Defined in the module as @code{(quaternion-equal number1 number2)}. Returns @code{#t} if the two arguments are equevilent mathematically, otherwise returns @code{#f}.
                                                                                                            If none of the arguments are quaternions, uses racket's default @code{eq?} function, otherwise it converts each argument into a quaternion, then compares each corresponding element of two quaternions left to right. The and of those comparisions is returned.  
                                                                                                            @codeblock|{
                                                                                                                        >(eq? (quaternion 2 5 34 -1) (quaternion 2 5 34 -1))
                                                                                                                        #t
                                                                                                                        >(eq? (quaternion 2 5 34 -1) (quaternion -1 0 4 16))
                                                                                                                        #f}|}

@defproc[(quaternion->string [x quaternion?/number?]) string?]{Returns @italic{x} as a string representation of a quaternion. Given in the form of @code{"(quaternion h i j k)"}.
                                                                       @codeblock|{ 
                                                                                   >(quaternion->string (quaternion 2 5 34 -1))
                                                                                   "(quaternion 2 5 34 -1)"}|}

@defproc[(make-quaternion [x quaternion?/number?]) quaternion?]{Only available within the module. Turns any number into a quaternion. If @italic{x} is a quaternion, returns @italic{x}. If @italic{x} is a number, returns @code{(quaternion (real-part x) (imag-part x) 0 0)}. If neither, returns @code{quaternion 0 0 0 0}.
                                                                                                                                         @codeblock|{
                                                                                                                                                     >(make-quaternion 2+3i)
                                                                                                                                                     (quaternion 2 3 0 0)}|}


@defproc[(unmake-quaternion [x quaternion?]) number?/quaternion?]{Only available within the module. If @code{(quaternion-j x)} and @code{(quaternion-k x)} are zero, returns a complex number, otherwise returns @italic{x}.
                                                                                                       @codeblock|{ 
                                                                                                                   >(unmake-quaternion (quaternion 2 3 0 0))
                                                                                                                   2+3i}|}

@defproc[(scalar-part [x quaternion?/number?]) number?]{Returns the real part of the input as a real number. For quaternions it is equivalent to @code{(quaternion-h x)}.
                                                                                                                                                 @codeblock|{ 
                                                                                                                                                             >(scalar-part (quaternion 2 5 34 -1))
                                                                                                                                                             2}|}

@defproc[(vector-part [x quaternion?/number?]) quaternion?]{Returns the imaginary part of the input as a quaternion with real part 0. It does this by converting the input to a quaternion, then subtracting the real part.
                                                            @codeblock|{
                                                                        >(vector-part (quaternion 2 5 34 -1))
                                                                        (quaternion 0 5 34 -1)}|}

@defproc[(reciprocal [w quaternion?/number?])quaternion?/number?]{Returns the reciprocal of @italic{w}. This is equivalent @code{(/ x)} or @code{(/ 1 x)}. 
                                                                                          @codeblock|{
                                                                                                      >(conjugate (quaternion 2 5 34 -1))
                                                                                                      (quaternion  2 -5 -34 1)}|}

@defproc[(conjugate [w quaternion?/number?])quaternion?/number?]{Returns the conjugate of @italic{w}. This is equivalent to (- (real-part w) (vector-part w))]. If none of the arguments are quaternions, uses rackets default conjugate procedure.
                                                                                          @codeblock|{
                                                                                                      >(conjugate (quaternion 2 5 34 -1))
                                                                                                      (quaternion  2 -5 -34 1)}|}

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


