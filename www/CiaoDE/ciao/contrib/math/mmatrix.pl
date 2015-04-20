:- module(mmatrix,[
	lsum/2,
	lsum2/2,
	mtrans/2,
	mmultiply/3,
	mmultiplyt/3,
	vmultiply/3,
	rmultiply/3,
	rdiv/3,
	rmuladd/4,
	vadd/3,
	lvsum/3,
	vsub/3,
	madd/3,
	mreverse/2,
	msolve/3,
	mmsolve/3,
	mtriang_to_rect/2
],[assertions, regtypes]).

:- use_module(library(lists)).

:- comment(title, "Matrix operations").

:- comment(author, "Edison Mera").

:- comment(module, "A complete package of vector and matrix
   operations.  For use with the statistic library.").

%:- entry mmultiply(X,Y,Z): ( var(Z), list(X,list(num)), list(X,list(num)) ). 
:- entry mmultiply(X,Y,Z): ( var(Z), ground(X), ground(Y)).

% :- regtype vector(X) # "@var{X} is a vector of numbers.".

% vector(X) :- list(X,number).

% :- regtype matrix(X) # "@var{X} is a matrix, i.e., a vector of vector of numbers.".

% matrix(X) :- list(X,vector).

:- true pred lsum(List,Sum): list(number) * term => list(number) *
   number # "Unifies @var{Sum} with the total sum of the numbers in
   the list @var{List}.".

lsum([],0).
lsum([X|Xs],S):-
	lsum(Xs, S1),
	S is X + S1.

:- true pred lsum2(List,Sum2): list(number) * term => list(number) *
   number # "Unifies @var{Sum2} with the total sum of the square of
   the numbers in the list @var{List}.".

lsum2([],0).
lsum2([X|Xs],S) :-
	lsum2(Xs, S1),
	S is X*X + S1.

:- true pred vmultiply(Vector1,Vector2,Result): list(number) *
   list(number) * term => list(number) * list(number) * number #
   "Unifies @var{Result} with the scalar product between the vectors
   @var{Vector1} and @var{Vector2}.".

%scalar product between two vectors
vmultiply([],[],0).
vmultiply([H1|T1], [H2|T2], Result):- 
	vmultiply(T1,T2, Newresult), 
	Product is H1*H2,
	Result is Product+Newresult.

:- true pred addcol(Vector,Matrix,Result): list(number) *
   list(list(number)) * term => list(number) * list(list(number)) *
   list(list(number)) # "Unifies @var{Result} with the matrix obtained
   adding @var{Vector} as the first column of @var{Matrix}.".

addcol([],[],[]).
addcol([A|As],[],[[A]|Cs]) :-
	addcol(As,[],Cs).
addcol([A|As],[B|Bs],[[A|B]|Cs]) :-
	addcol(As,Bs,Cs).

:- true pred mtrans(Matrix,Trans): list(list(number)) * term =>
   list(list(number)) * list(list(number)) # "Unifies @var{Trans} with
   the transposed matrix of @var{Matrix}.".

mtrans([],[]).
mtrans([V|Ms],T) :-
	mtrans(Ms,Xs),
	addcol(V,Xs,T).

:- true pred mmultiplyt(Matrix1,Matrix2,Result) : list(list(number)) *
   list(list(number)) * term => list(list(number)) *
   list(list(number)) * list(list(number)) # "Unifies @var{Result}
   with the matricial product between the matrices @var{Matrix1} and
   the transposed matrix of @var{Matrix2}.".

mmultiplyt([],_,[]).
mmultiplyt([V0|Rest], V1, [Result|Others]):-  
	mmultiplyt(Rest, V1, Others),
	multiplyt(V1,V0,Result).

multiplyt([],_,[]).
multiplyt([V0|Rest], V1, [Result|Others]):-  
	multiplyt(Rest, V1, Others),
	vmultiply(V0,V1,Result).

:- true pred mmultiply(Matrix1,Matrix2,Result): list(list(number)) *
   list(list(number)) * term => list(list(number)) *
   list(list(number)) * list(list(number)) # "Unifies @var{Result}
   with the matricial product between the matrices @var{Matrix1} and
   @var{Matrix2}.".

mmultiply(A,B,C) :-
	mtrans(B,B2),
	mmultiplyt(A,B2,C).

:- true pred rmultiply(Scalar,Vector,Result): number * list(number) *
   term => number * list(number) * list(number) # "Unifies
   @var{Result} with the scalar product between @var{Scalar} and
   @var{Vector}.".

rmultiply(_,[],[]).
rmultiply(R,[X|Xs],[Y|Ys]) :-
 	Y is R*X,
 	rmultiply(R,Xs,Ys).

:- true pred rdiv(Scalar,Vector,Result): number * list(number) * term
   => number * list(number) * list(number) # "Unifies @var{Result}
   with the scalar product between 1.0/@var{Scalar} and
   @var{Vector}.".

rdiv(_,[],[]).
rdiv(R,[X|Xs],[Y|Ys]) :-
	Y is X/R,
	rdiv(R,Xs,Ys).

:- true pred rmuladd(Scalar,Mul,Add,Result): number * list(number) *
   list(number) * term => number * list(number) * list(number) *
   list(number) # "Unifies @var{Result} with the scalar product
   between @var{Scalar} and @var{Mul}, plus @var{Add}.  In other
   words, @var{Result}=@var{Scalar}*@var{Mul}+@var{Add}.".

rmuladd(_,[],B,B).
rmuladd(R,[A|As],[B|Bs],[C|Cs]) :-
	C is R*A+B,
	rmuladd(R,As,Bs,Cs).

:- true pred vadd(X,Y,Z): list(number) * list(number) * term =>
   list(number) * list(number) * list(number) # "Unifies @var{Z} with
   the sum of the vectors @var{X} and @var{Y}.".

vadd([],Y,Y).
%vadd(X,[],X).
vadd([X|Xs],[Y|Ys],[Z|Zs]) :-
	Z is X+Y,
	vadd(Xs,Ys,Zs).

lvsum([],     V,  V).
lvsum([D|Ds], V0, V) :-
	vadd(V0, D, V1),
	lvsum(Ds, V1, V).

:- true pred vsub(X,Y,Z): list(number) * list(number) * term =>
   list(number) * list(number) * list(number) # "Unifies @var{Z} with
   the rest of the vectors @var{X} and @var{Y}.".

vsub([],Y,Y).
% vsub(X,[],X).
vsub([X|Xs],[Y|Ys],[Z|Zs]) :-
	Z is X-Y,
	vsub(Xs,Ys,Zs).

:- true pred madd(A,B,C): list(list(number)) * list(list(number)) *
   term => list(list(number)) * list(list(number)) *
   list(list(number)) # "Unifies @var{Z} with the sum of the matrices
   @var{X} and @var{Y}.".

madd([],Y,Y).
% madd(X,[],X).
madd([X|Xs],[Y|Ys],[Z|Zs]) :-
	vadd(X,Y,Z),
	madd(Xs,Ys,Zs).

pivot([_|_],_,[],[],[],[]).
pivot([A|Ap],Bp,[[A1|Ar]|As],[B|Bs],[C|Cs],[D|Ds]) :-
	R is -A1/A,
	rmuladd(R,Ap,Ar,C),
	D is R*Bp+B,
	pivot([A|Ap],Bp,As,Bs,Cs,Ds).

selpivot([A],[B],A,B,[],[]).
selpivot([[A|Ar]|As],[B|Bs],Ap,Bp,C,D) :-
	selpivot(As,Bs,[Ap1|Ap1s],Bp1,C1,D1),
	(
	    abs(A) >= abs(Ap1) ->
	    Ap=[A|Ar],
	    Bp=B,
	    C=[[Ap1|Ap1s]|C1],
	    D=[Bp1|D1];
	    Ap=[Ap1|Ap1s],
	    Bp=Bp1,
	    C=[[A|Ar]|C1],
	    D=[B|D1]
	).

reduce([],[],[],[]).
reduce([A|As],[B|Bs],[C|Cs],[D|Ds]) :-
	selpivot([A|As],[B|Bs],C,D,As2,Bs2),
	pivot(C,D,As2,Bs2,C1,D1),
	reduce(C1,D1,Cs,Ds).

reduceback(_,[],[],[],[]).
reduceback(R,[[A|Ar]|As],[B|Bs],[Ar|Cs],[D|Ds]) :-
	D is B-R*A,
	reduceback(R,As,Bs,Cs,Ds).

eliminate([],[],[]).
eliminate([[A]|As],[B|Bs],[C|Cs]) :-
	C is B/A,
	reduceback(C,As,Bs,Cs1,Ds1),
	eliminate(Cs1,Ds1,Cs).

:- true pred mreverse(A,B): list(list(number)) * term =>
   list(list(number)) * list(list(number)) # "Takes the matrix
   @var{A}, reverses the order of the columns, and unifies it with
   @var{B}.".

mreverse([],[]).
mreverse([A|As],[B|Bs]) :-
	reverse(A,B),
	mreverse(As,Bs).

:- true pred msolve(A,B,X): list(list(number)) * list(number) * term
   => list(list(number)) * list(number) * list(number) # "Solve
   @var{X} in the linear system @var{A}*@var{X}=@var{B}, where @var{B}
   is a vector.".

msolve(A,B,X) :-
	reduce(A,B,A1,B1),
	mreverse(A1,A2),
	reverse(A2,A3),
	reverse(B1,B2),
	eliminate(A3,B2,X1),
	reverse(X1,X).

mpivot([_|_],_,[],[],[],[]).
mpivot([A|Ap],Bp,[[A1|Ar]|As],[B|Bs],[C|Cs],[D|Ds]) :-
	R is -A1/A,
	rmuladd(R,Ap,Ar,C),
	rmuladd(R,Bp,B,D),
	mpivot([A|Ap],Bp,As,Bs,Cs,Ds).

mreduce([],[],[],[]).
mreduce([A|As],[B|Bs],[C|Cs],[D|Ds]) :-
	selpivot([A|As],[B|Bs],C,D,As2,Bs2),
	mpivot(C,D,As2,Bs2,C1,D1),
	mreduce(C1,D1,Cs,Ds).

mreduceback(_,[],[],[],[]).
mreduceback(R,[[A|Ar]|As],[B|Bs],[Ar|Cs],[D|Ds]) :-
	rmuladd(-A,R,B,D),
	mreduceback(R,As,Bs,Cs,Ds).

meliminate([],[],[]).
meliminate([[A]|As],[B|Bs],[C|Cs]) :-
	rdiv(A,B,C),
	mreduceback(C,As,Bs,Cs1,Ds1),
	meliminate(Cs1,Ds1,Cs).

:- true pred mmsolve(A,B,X): list(list(number)) * list(list(number)) *
   term => list(list(number)) * list(list(number)) *
   list(list(number)) # "Solve @var{X} in the linear system
   @var{A}*@var{X}=@var{B}, where @var{B} is a matrix.".

mmsolve(A,B,X) :-
	mreduce(A,B,A1,B1),
	mreverse(A1,A2),
	reverse(A2,A3),
	reverse(B1,B2),
	meliminate(A3,B2,X1),
	reverse(X1,X).

addcolumnzero([],[],[]).
addcolumnzero([A|As],[[0|A]|Bs],[0|Cs]) :-
	addcolumnzero(As,Bs,Cs).

midentity(0,[]).
midentity(N,[[1|I]|Is]) :-
	N1 is N-1,
	midentity(N1,Is1),
	addcolumnzero(Is1,Is,I).

minverse(A,B) :-
	length(A,N),
	midentity(N,I),
	mmsolve(A,I,B).

:- true pred mtriang_to_rect(A,B): list(list(number)) *
   list(list(number)) => list(list(number)) * list(list(number)) #
   "Converts the triangular matrix @var{A} to the rectangular matrix
   @var{B}.  Note that a triangular matrix is as follow:

   A = [[A11,A12,    ...,A1N],
            [A22,A23,...,A2N],
                         ....
                        [ANN]]

   And the respective rectangular matrix is

   B = [[A11,A12,    ...,A1N],
        [A12,A22,A23,...,A2N],
        [A13,A23,A33,...,A3N],
	                  ...
        [A1N,A2N,A3N,...,ANN]]

".

mtriang_to_rect([],[]).
mtriang_to_rect([[A|Ar]|As],[[A|Ar]|Bs]) :-
	mtriang_to_rect(As,Bs1),
	addcol(Ar,Bs1,Bs).
