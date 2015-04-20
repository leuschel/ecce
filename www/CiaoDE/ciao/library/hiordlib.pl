:- module(hiordlib, [map/3,foldl/4,minimum/3], 
                    [assertions,basicmodes,functions,hiord]).

:- comment(title,"Higher-order predicates").

:- comment(author,"Daniel Cabeza").
:- comment(author,"Manuel Carro").

:- comment(module,"This library implements a few basic higher-order
   predicates. These add functionality to the basic 
   higher-order functionality of Ciao. Examples of the latter are:


   Using pred(1):

@begin{verbatim}
  list(L, functor(_,2))
  list(L, >(0))
@end{verbatim}

   Using pred(2):

").

:- pred map(LList,Op,RList) # "Examples of use:
@begin{verbatim}
  map([1,3,2], arg(f(a,b,c,d)), [a,c,b]) or
  map([1,3,2], nth([a,b,c,d]), [a,c,b])
  map([\"D\",\"C\"], append(\".\"), [\"D.\",\"C.\"])
@end{verbatim}
".

:- meta_predicate map(_,pred(2),_).

map([], _) := [].
map([X|Xs], P) := [~P(X)|~map(Xs,P)].

:- pred foldl(List,Seed,Op,Result) # "Example of use:
@begin{verbatim}
?- foldl([\"daniel\",\"cabeza\",\"gras\"], \"\", 
         (''(X,Y,Z) :- append(X, \" \"||Y, Z)), R).

R = \"daniel cabeza gras \" ? 
@end{verbatim}
".

:- meta_predicate foldl(_,_,pred(3),_).

foldl([], Seed, _Op) := Seed.
foldl([X|Xs], Seed, Op) := ~Op(X,~foldl(Xs,Seed,Op)).

:- meta_predicate minimum(_, pred(2), _).

:- pred minimum(?List, +SmallerThan, ?Minimum) : list * callable * term
# "@var{Minimum} is the smaller in the nonempty list @var{List}
according to the relation @var{SmallerThan}: @pred{SmallerThan(X, Y)}
succeeds iff X is smaller than Y.".

minimum([X|Xs], Pred, Min):- minimum_carry(Xs, Pred, X, Min).
minimum_carry([], _Pred, M, M).
minimum_carry([X|Xs], Pred, MinSoFar, Min):-
        (
            Pred(MinSoFar, X) ->
            minimum_carry(Xs, Pred, MinSoFar, Min)
        ; 
            minimum_carry(Xs, Pred, X, Min)
        ).
