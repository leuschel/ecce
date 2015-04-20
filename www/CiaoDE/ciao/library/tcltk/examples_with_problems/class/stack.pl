:- class(stack).

:- data storage/1.

:- export(push/1).


push(Item) :-
	nonvar(Item),
	asserta_fact(storage(Item)).
