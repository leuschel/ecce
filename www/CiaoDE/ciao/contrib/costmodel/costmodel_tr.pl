:- module(costmodel_tr, [costmodel_def/3],[assertions]).

:- use_module(library(lists)).
:- use_module(library(aggregates)).

costmodel_def(end_of_file, Clauses, _M) :-
	Clauses = [(:- multifile(costmodel/4)),
	end_of_file].
costmodel_def((:- costmodel('::'(Pred, '=>'(Dist, Model)))), Clauses, M) :-
	Clauses = [(costmodel(M, Pred, Dist, Model))].
%	assertz_fact(cost(Pred, Dist, Model)).
