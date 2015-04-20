
:- class(canvas).


:- data storage/1.

:- export(push/1).
%:- export(pop/1).
%:- export(top/1).
%:- export(is_empty/0).

push(I) :- 
	nonvar(I),
	asserta_fact(storage(I)).
