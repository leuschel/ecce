:- module(lazy_lib,
	[
	    nums_from/2,
	    nums_from_inc/3,
 	    repeat/2,
 	    cycle/2, 
 	    take/3,
 	    takeWhile/3,
 	    drop/3,
 	    dropWhile/3,
 	    splitAt/3,
 	    span/3,
 	    tail/2,
	    lazy_map/3,
	    lazy_foldl/4,
 	    zipWith/4
	],
	[functions, assertions, basicmodes, regtypes, hiord, lazy]).

:- comment(title,"Lazy evaluation library").
:- comment(author,"Amadeo Casas Cuadrado").
:- comment(author, "@tt{http://www.cs.unm.edu/~amadeo}").
:- comment(author, "University of New Mexico").
:- comment(usage,"This module provides several predicates that make easier
   to develop predicates that will be executed lazily.").


:- pred nums_from(+X,-List): int * var => int * list(int) # "@var{List} is
   unified with an infinite list of successive numbers starting in
   @var{X}".

:- lazy function nums_from/1.
nums_from(X) := [X | nums_from(X+1)].

:- pred nums_from_inc(+X,+Y,-List): int * int * var => int * int *
   list(int) # "@var{List} is unified with an infinite list of successive
   numbers starting in @var{X} with an increment of @var{Y}".

:- lazy function nums_from_inc/2.
nums_from_inc(X, Y) := [X | nums_from_inc(X+Y, Y)].

:- pred repeat(+X,-List): term * var => term * list(term) # "@var{List} is
   unified with an infinite list of the term @var{Y}".

:- lazy function repeat/1.
repeat(X) := [X | repeat(X)].

:- pred cycle(+X,-List): term * var => term * list(term) # "@var{List} is
   unified with an infinite list of the term @var{Y} repeated infinite
   times".

:- lazy function cycle/1.
:- lazy function cycle_/2.
cycle(X) := cycle_(X, cycle(X)).
cycle_([], X)    := X.
cycle_([H|T], Y) := [H | cycle_(T, Y)].

:- pred take(+X,+ListA,-ListR): counter * list(term) * term => counter *
   list(term) * list(term) # "@var{ListR} is unified with the first @var{X}
   elements of the infinite list @var{ListA}".

:- function take/2.
take(0, _)     := [].
take(X, [H|T]) := [H | take(X-1, T)] :- X > 0.

:- pred takeWhile(+P,+ListA,-ListR): callable * list(term) * term =>
   callable * list(term) * list(term) # "@var{ListR} is unified with the
   first elements of the infinite list @var{ListA} while the condition
   @var{P} is true".

:- function takeWhile/2.
takeWhile(P, [H|T]) := P(H) ? [H | takeWhile(P, T)]
                     | [].

:- pred drop(+X,+ListA,-ListR): counter * list(term) * term => counter *
   list(term) * list(term) # "@var{ListR} is unified with the infinite list
   @var{ListA} dropping the first @var{X} elements".

:- lazy function drop/2.
drop(0, List)  := List.
drop(X, [_|T]) := drop(X-1, T).

:- pred dropWhile(+P,+ListA,-ListR): callable * list(term) * term =>
   callable * list(term) * list(term) # "@var{ListR} is unified with the
   infinite list @var{ListA} dropping the first elements while the
   condition @var{P} is true".

:- lazy function dropWhile/2.
dropWhile(P, [H|T]) := dropWhile(P, T) :- P(H), !.
dropWhile(_, List)  := List.

:- pred splitAt(+X,+ListA,-Res): counter * list(term) * term => counter *
   list(term) * (list(term), list(term)) # "@var{Res} is unified with a
   tuple of lists where the first list is composed by the first @var{X}
   elements of the list @var{ListA} and the second list is composed by the
   rest of the elements of @var{ListA}".

:- function splitAt/2.
splitAt(0, List)  := ([], List).
splitAt(_, [])    := ([], []).
splitAt(X, [H|T]) := ([H|T1], T2) :- X > 0, (T1, T2) = splitAt(X-1, T).

:- pred span(+P,+ListA,-Res): callable * list(term) * term => callable *
   list(term) * (list(term), list(term)) # "@var{Res} is unified with a
   tuple of lists where the first list is composed by the elements of
   @var{ListA} which verify the condition @var{P} and the second list is
   composed by the rest of the elements of the initial list".

:- function span/2.
span(_, [])    := ([], []).
span(P, [H|T]) := ([H|T1], T2) :- P(H), !, (T1, T2) = span(P, T).
span(P, [H|T]) := (T1, [H|T2]) :- (T1, T2) = span(P, T).

:- pred tail(+ListA,-ListR): list(term) * term => list(term) * list(term) #
   "@var{ListR} is unified with the tail of the infinite list @var{ListA}".

:- lazy function tail/1.
tail([_|T]) := T.

:- pred lazy_map(+ListA,+P,-ListR): list(term) * callable * term =>
   list(term) * callable * list(term) # "Version of the map/3 predicate to
   be executed lazily".

:- meta_predicate lazy_map(_,pred(2),_).

:- lazy function lazy_map/2.
lazy_map([], _)     := [].
lazy_map([X|Xs], P) := [~P(X) | lazy_map(Xs, P)].

:- pred lazy_foldl(+ListA,+X,+P,-ListR): list(term) * term * callable *
   term => list(term) * term * callable * list(term) # "Version of the
   foldl/3 predicate to be executed lazily".

:- meta_predicate lazy_foldl(_,_,pred(3),_).

:- lazy function lazy_foldl/2.
lazy_foldl([], Seed, _Op)    := Seed.
lazy_foldl([X|Xs], Seed, Op) := ~Op(X, ~lazy_foldl(Xs, Seed, Op)).

:- pred zipWith(+P,+ListA,+ListB,-ListR): callable * list(term) *
   list(term) * term => callable * list(term) * list(term) * list(term) #
   "@var{ListR} is a list whose elements are calculated from the function
   @var{P} and the elements of input lists @var{ListA} and @var{ListB}
   occuring at the same position in both lists".

:- meta_predicate zipWith(pred(3),_,_,_).

:- lazy function zipWith/3.
zipWith(_, [], [])           := [].
zipWith(F, [H1|L1], [H2|L2]) := [~F(H1, H2) | zipWith(F, L1, L2)].

%-------------------------------------------------------------------------

:- regtype counter(C) #"@var{C} is a counter.".

counter(Id):- nnegint(Id).
