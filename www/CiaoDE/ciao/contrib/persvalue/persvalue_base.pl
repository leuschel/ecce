:- module(_,_,[]).

:- use_module(library(lists)).

get_value_from_string(String, Name, Value) :-
	list_concat(["\n",Rest], String) ->
	get_value_from_string(Rest, Name, Value)
 ;
	(   list_concat(["#", _Comment, "\n", Rest], String) ->
	    get_value_from_string(Rest, Name, Value)
	;
	    (
		list_concat([Name1,"=",Value1,"\n",Rest], String) ->
		(
		    Name=Name1,
		    Value=Value1
		;
		    get_value_from_string(Rest, Name, Value)
		)
	    ;
		list_concat([Name,"=",Value], String)
	    )
	).
