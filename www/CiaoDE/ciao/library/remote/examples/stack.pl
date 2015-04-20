:- class(stack, [], []).

:- export([push/1, pop/1, top/1, empty/0]).

:- data storage/1.
storage(a).
storage(b).
storage(c).


push(X) :-
	asserta(storage(X)).

pop(X) :-
	retract(storage(X)).

top(X) :-
	storage(X).

empty :-
	storage(_),!,
	fail.
empty .
