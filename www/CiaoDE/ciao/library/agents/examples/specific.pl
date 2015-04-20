:- class(specific).

:- inherit_class(generic).

:- export(smain/0).

specific :- generic.

smain :- display(specific),nl,
	inherited main.