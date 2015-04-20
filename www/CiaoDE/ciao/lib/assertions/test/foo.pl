
:- module(foo,[main/0],[iso]).

:- multifile issue_debug_messages/1.
:- data issue_debug_messages/1.

issue_debug_messages(foo).

:- use_module(library(messages)).

main :-
	debug_goal(Y is 3+5,"Three plus Five is ~w",[Y]).
