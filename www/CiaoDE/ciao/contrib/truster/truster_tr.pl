:- module(truster_tr, [truster_def/3],[assertions]).

:- use_module(library(lists)).
:- use_module(library(aggregates)).

:- data truster/2.

truster_def(0, [(:- initialization(testall))], _M).
truster_def(end_of_file, Clauses, _M) :-
	findall((truster(Query, Goal)), truster(Query, Goal), Clauses0),
	append(Clauses0, [(:- multifile truster/2),
	(   testall :-
	    truster(Query, Goal),
	    (   call(Query) ->
		(   Query = Goal ->
		    true
		;
		    warning(['{The next test could not be passed: ', Query, '=', Goal, '}'])
		),
		false
	    ;
		warning(['{The next test fail: ', Query, '}'])
	    )
	),
	(:- initialization(testall)),
	(:- multifile(costmodel/4)),
	end_of_file], Clauses),
	retractall_fact(truster(Query, Goal)).
truster_def((:- truster('->'(Query, Goal))), [], _M) :-
	assertz_fact(truster(Query, Goal)).
truster_def((:- truster(Query)), [], _M) :-
	assertz_fact(truster(Query,_)).
