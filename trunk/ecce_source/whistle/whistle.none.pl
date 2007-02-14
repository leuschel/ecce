:- module('whistle.none',['whistle.none:whistle'/4]).

:- set_prolog_flag(single_var_warnings,off).

%:- dynamic whistle/4.

'whistle.none:whistle'(GoalID,Goal,Chtree,WhistlGoalID) :-
	fail.
