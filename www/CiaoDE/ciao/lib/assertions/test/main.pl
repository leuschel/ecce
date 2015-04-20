
:- module(main,[main/0,r/3,tree/1,foo/1],
	[assertions,isomodes,functions,regtypes,hiord]).

:- use_module(aux1,[p/1]).

:- use_module(arith,[foo/2]).

:- pred r(@X,+int,gnd) : var(X) => integer(X) # "Nice pred.".

:- trust comp r/3 : int*int*var + succeeds ; "Suceeds.".

:- pred foo(+int).

:- modedef main_functor(A,X) : ( A=X(a) ).

:- pred bar(main_functor(f)).

main :-
	p(X),
	display(X).

:- prop foo(X) # "@var{X} is any term.".

foo(_).

%% :- prop tree(X) + regtype # "@var{X} is a tree.".
%% 
%% tree(void).
%% tree(t(_,L,R)) :- tree(L), tree(R).

:- true pred =\=(+arithexpression,+arithexpression) + iso.

:- true regtype tree(X) # "@var{X} is a tree.".

tree := void.
tree := t(_,~tree,~tree).

:- impl_defined(foo/1).
