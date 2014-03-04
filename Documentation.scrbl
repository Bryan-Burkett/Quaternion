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
@defstruct[quaternion ([h real?] [i real?] [j real?] [k real?])]{A structure to represent a quaternion. @code{h} represents the real part, @code{i}, @code{j} and @code{k} represent the imaginary parts. If not provided with 4 real numbers, return a type-wrong-format error. Automatically defines @code{(quaternion? x)} to test if @code{x}
                                                                 is a quaternion. @code{(quaternion-h x)}, @code{(quaternion-i x)}, @code{(quaternion-j x)} and @code{(quaternion-k x)} can be used to get the components of a quaternion @code{x}.}


@defproc[(eq? [x quaternion?/number?][y quaternion?/number?]) quaternion?/number?]{Defined in the module as @code{(quaternion-equal number1 number2)}. Returns @code{#t} if the two arguments are equevilent mathematically, otherwise returns @code{#f}.
                                                                                   If none of the arguments are quaternions, uses racket's default @code{eq?} function, otherwise it converts each argument into a quaternion, then compares each corresponding element of two quaternions left to right. The and of those comparisions is returned.  
                                                                                   @codeblock|{
                                                                                               >(eq? (quaternion 2 5 34 -1) (quaternion 2 5 34 -1))
                                                                                               #t
                                                                                               >(eq? (quaternion 2 5 34 -1) (quaternion -1 0 4 16))
                                                                                               #f}|}

@defproc[(quaternion->string [x quaternion?/number?]) string?]{Returns @code{x} as a string representation of a quaternion. Given in the form of @code{"(quaternion h i j k)"}.
                                                                       @codeblock|{ 
                                                                                   >(quaternion->string (quaternion 2 5 34 -1))
                                                                                   "(quaternion 2 5 34 -1)"}|}

@defproc[(make-quaternion [x quaternion?/number?]) quaternion?]{Only available within the module. Turns any number into a quaternion. If @code{x} is a quaternion, returns @code{x}. If @code{x} is a number, returns @code{(quaternion (real-part x) (imag-part x) 0 0)}. If neither, returns @code{quaternion 0 0 0 0}.
                                                                @codeblock|{
                                                                            >(make-quaternion 2+3i)
                                                                            (quaternion 2 3 0 0)}|}


@defproc[(unmake-quaternion [x quaternion?]) number?/quaternion?]{Only available within the module. If @code{(quaternion-j x)} and @code{(quaternion-k x)} are zero, returns a complex number that is mathematically equivalent to @code{x}, otherwise returns @code{x}.
                                                                  @codeblock|{ 
                                                                              >(unmake-quaternion (quaternion 2 3 0 0))
                                                                              2+3i}|}

@defproc[(scalar-part [x quaternion?/number?]) number?]{Returns the real part of @code{x} as a real number. For quaternions it is equivalent to @code{(quaternion-h x)}.
                                                        @codeblock|{ 
                                                                    >(scalar-part (quaternion 2 5 34 -1))
                                                                    2}|}

@defproc[(vector-part [x quaternion?/number?]) quaternion?]{Returns the imaginary part(s) of @code{x} as a quaternion with real part 0. It does this by converting the input to a quaternion, then subtracting the real part.
                                                            @codeblock|{
                                                                        >(vector-part (quaternion 2 5 34 -1))
                                                                        (quaternion 0 5 34 -1)}|}

@defproc[(norm [x quaternion?])number?]{Defined within the module as @code{(quaternion-norm number)}. Returns the norm of @code{x}. Also known as the magnitude, this represents the lenghth of the quaternion when represented as a vector. Found by taking the square root of the sum of the squares of the components of @code{x}.
                                                                     @codeblock|{ 
                                                                                 >(norm (quaternion 2 5 34 -1))
                                                                                 34.4383507154451252}|} 
                                                                                                       
@defproc[(conjugate [x quaternion?/number?])quaternion?/number?]{Defined within the module as @code{(quaternion-conjugate number)}. Returns the conjugate of @code{x}. This is a new quaternion with the same real part as @code{x} and the opposite vector parts. The product of @code{x} and it's reciprocal is the square of the norm. This a new quaternion with the same real part as @code{x} 
                                                                 This is equivalent to @code{(- (real-part x) (vector-part x))}, however this function takes the opposite of each imaginary component individually. If none of the arguments are quaternions, uses racket's default @code{conjugate} procedure.
                                                                 @codeblock|{
                                                                             >(conjugate (quaternion 2 5 34 -1))
                                                                             (quaternion  2 -5 -34 1)}|}
                                                                                                       
                                                                                                       
                                                                                                       
@defproc[(reciprocal [x quaternion?/number?])quaternion?/number?]{Defined within the module as @code{(quaternion-reciprocal number)}. Returns the reciprocal of @code{x}. A reciprocal is defined such that the reciprocal of @code{x} times @code{x} is 1. This procedure returns a new quaternion whose elements are the corresponding elements of @code{x} is divided by the norm of @code{x} squared. 
                                                                  @codeblock|{
                                                                              >(reciprocal (quaternion 2 5 34 -1))
                                                                              (quaternion 0.0016863406408094439 -0.004215851602023609 -0.028667790893760547 0.0008431703204047219)}|}

@defproc[(unit [w quaternion?])number?]{Defined within the module as @code{(quaternion-unit number)}. Returns the unit quaternion of @code{w}. This is the quaternion divided by its norm, it represents the direction of the quaternion with a parallel quaternion of norm 1.
                                        @codeblock|{
                                                    >(unit (quaternion 2 5 34 -1))
                                                    (quaternion 0.0016863406408094439 -0.004215851602023609 -0.028667790893760547 0.0008431703204047219)}|}



@subsection{Basic Operators}

@defproc[(+ [x quaternion?/number?]...) quaternion?/number?]{Defined within the module as @code{(quaternion-add . quaternions)}. Returns the sum of all @code{x}s, or for only one @code{x} returns @code{x}.
                                                             If none of the arguments are quaternions, uses racket's default @code{+} procedure. Otherwise, converts all arguments to quaternions and returns a new quaternion whose components are the the sums of the corresponding components of the arguments.
                                                             @codeblock|{
                                                                         >(+ (quaternion 2 5 34 -1) (quaternion -1 0 4 16))
                                                                         (quaternion 1 5 38 15)
                                                                         > (+ (quaternion 1 2 3 4) 4 (quaternion 4 3 5 -3) 3-2i)
                                                                         (quaternion 12 3 8 1)}|}


@defproc*[([(- [x quaternion?/number?])quaternion?/number?]
[(- [x quaternion?/number?][w quaternion?/number?]...)quaternion?/number?])]{Defined within the module as @code{(quaternion-subtract . quaternions)}. Returns the difference of @code{w} from @code{x}. If no @code{w}s are supplied, returns @code{(- 0 x)} For multiple @code{w}s takes the difference of each @code{w} from @code{x}. If none of the parameters are quaternions, uses racket's default @code{-} procedure, otherwise makes each argument a quaternion,
                                                                                   then returns a new quaternion whose elements are the correspoonding elements of @code{x} minus the corresponding element of each @code{w}.
                                                                                   @codeblock|{
                                                                                               > (- (quaternion 1 2 3 4))
                                                                                               (quaternion -1 -2 -3 -4)
                                                                                               > (- (quaternion 3 2 4 5) (quaternion 5 3 7 5))
                                                                                               (quaternion -2 -1 -3 0)
                                                                                               > (- (quaternion 10 6 22 -5) 4 (quaternion 2 3 4 1) 3+2i)
                                                                                               (quaternion 1 1 18 -6)}|}

@defproc[(quaternion-multiply2 [x quaternion?][w quaternion?]) quaternion?]{Only defined within the module, used by the @code{*} and @code{/} procedures. Returns a quaternion product of two quaternions using the following formulae:
                                                                            @para{h@subscript{new} = h@subscript{x}*h@subscript{w} - i@subscript{x}*i@subscript{w} - j@subscript{x}*j@subscript{w} - k@subscript{x}*k@subscript{w}}
                                                                            @para{i@subscript{new} = h@subscript{x}*i@subscript{w} + i@subscript{x}*h@subscript{w} + j@subscript{x}*k@subscript{w} - k@subscript{x}*j@subscript{w}}
                                                                            @para{j@subscript{new} = h@subscript{x}*j@subscript{w} - i@subscript{x}*k@subscript{w} + j@subscript{x}*h@subscript{w} + k@subscript{x}*i@subscript{w}}
                                                                            @para{k@subscript{new} = h@subscript{x}*k@subscript{w} + i@subscript{x}*j@subscript{w} - j@subscript{x}*i@subscript{w} + k@subscript{x}*h@subscript{w}}}
                                                                                                                        }

@defproc[(* [x quaternion?/number?]...) quaternion?/number?]{Defined within the module as @code{(quaternion-multiply . quaternions)}. Returns the product of all @code{x}s, or for only one @code{x} returns @code{x}.
                                                             If none of the arguments are quaternions, uses racket's default @code{*} procedure. Otherwise, converts all arguments to quaternions and uses @code{quaternion-multiply2} to multiply all of the arguments pairwise from left to right.
                                                             @codeblock|{
                                                                         > (* (quaternion 1 2 3 4) (quaternion 4 3 2 1))
                                                                         (quaternion -12 6 24 12)
                                                                         > (* (quaternion 1 2 3 4) 2 (quaternion 4 3 2 1) 4-8i)
                                                                         (quaternion 0 240 0 480)}|}


@defproc*[([(/ [x quaternion?/number?])quaternion?/number?]
[(/ [x quaternion?/number?][w quaternion?/number?]...)quaternion?/number?])]{Defined within the module as @code{(quaternion-divide . quaternions)}. Returns @code{x} divided by @code{w}. If no @code{w}s are supplied returns @code{(reciprocal x)}, which is equivalent to @code{(/ 1 x)}. If none of the parameters are quaternions, uses racket's default @code{/} procedure.
                                                                             Otherwise.
                                                                            @codeblock|{
                                                                                        >(- (quaternion 2 5 34 -1) (quaternion -1 0 4 16))
                                                                                        (quaternion 3 5 30 -17)}|}

@subsection{Advanced Operators}


@defproc[(exp [x quaternion?/number?]) quaternion?/number?]{Returns the Euler's number to the power of @code{x}. If @code{x} is not a quaternion, uses racket's default exp procedure.
                                                                                                       @codeblock|{
                                                                                                                   >(+ (quaternion 2 5 34 -1) (quaternion -1 0 4 16))
                                                                                                                   (quaternion 1 5 38 15)}|}
@defproc[(expt [x quaternion?/number?][y quaternion?/number?]) quaternion?/number?]{Returns the @code{x} to the power of @code{y}. If if neither argument is a quaternion, uses racket's default expt procedure.
                                                                                                @codeblock|{
                                                                                                            >(+ (quaternion 2 5 34 -1) (quaternion -1 0 4 16))
                                                                                                            (quaternion 1 5 38 15)}|}


@defproc[(sin [x quaternion?/number?]) quaternion?/number?]{Returns the sine of @code{x}. If @code{x} is not a quaternion, uses racket's default sin procedure.
                                                                                @codeblock|{
                                                                                            >(+ (quaternion 2 5 34 -1) (quaternion -1 0 4 16))
                                                                                            (quaternion 1 5 38 15)}|}


@defproc[(cos [x quaternion?/number?]) quaternion?/number?]{Returns the cosine of @code{x}. If @code{x} is not a quaternion, uses racket's default cos procedure.
                                                                                  @codeblock|{
                                                                                              >(+ (quaternion 2 5 34 -1) (quaternion -1 0 4 16))
                                                                                              (quaternion 1 5 38 15)}|}


