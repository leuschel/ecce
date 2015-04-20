:- class(generic).

:- use_module(library(system),[pause/1,time/1]).

:- concurrent conc_name/1.
:- data data_name/1.

:- export(main/0).

generic :-
	asserta_fact(data_name(ljf)),
	display(constructor),nl.

main :-
	asserta_fact(data_name(adf)),
	asserta_fact(conc_name(fds)),
	retract_fact(data_name(X)),
	display(X),nl,
%	retract_fact(conc_name(X)),
	display(X),nl,
	pause(3),
	display(pause),nl,
	retract_fact(data_name(X)).
