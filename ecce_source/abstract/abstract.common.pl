:- module( 'abstract.common' , _ ).

:- use_module( '../bimtools' ).
:- use_module( '../homeomorphic' ).

get_full_split_indicator([],_Nr,[]).
get_full_split_indicator([_H|T],Nr,[Nr|FST]) :-
	Nr1 is Nr + 1,
	get_full_split_indicator(T,Nr1,FST).


construct_none_chtrees([],[]).
construct_none_chtrees([_H|T],[none|TN]) :-
	construct_none_chtrees(T,TN).






find_minimally_general_element([Min],Min).
find_minimally_general_element([M|Ms],Min) :-
  (not_minimal(M,Ms) ->
    find_minimally_general_element(Ms,Min);
    (Min = M)).

not_minimal(M,[split(M1,_CN,_R,_RN)|_]) :-
  strict_instance_of(M1,M).
not_minimal(M,[split(_M1,_CN,_R,_RN)|Ms]) :-
  not_minimal(M,Ms).
