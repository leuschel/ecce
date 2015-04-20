:- module(_, _, _).

:- use_module(library(prolog_sys)).
:- use_module(library(format)).
:- use_module(library(dict)).

speed :-
        statistics(runtime, _),
        assrt,
        statistics(runtime, [_|T]),
        display('assrt '),
        display(T),
        display(' milliseconds'),
        nl,
        statistics(runtime, _),
        dict,
        statistics(runtime, [_|T2]),
        display('dict '),
        display(T2),
        display(' milliseconds'),
        nl.

% With 40 different keys:
%  assrt [2093] milliseconds
%  dict [1824] milliseconds
% With 100000 different keys:
%  assrt [8138] milliseconds
%  dict [6410] milliseconds
% With 200000 different keys:
%  assrt [15976] milliseconds
%  dict [7119] milliseconds
% With 400000 different keys:
%  assrt [20683] milliseconds
%  dict [8312] milliseconds

:- use_module(library(random)).

:- data k/2.

assrt :-
	assrt_2(1000000).
	
assrt_2(0) :- !.
assrt_2(N) :-
	random(1, 700000, Key),
	( current_fact(k(Key,_)) ->
	    true
	; asserta_fact(k(Key,[a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a]))
	),
	N1 is N - 1,
	assrt_2(N1).

dict :- dict_2(1000000, _).

dict_2(0, _) :- !.
dict_2(N, Dic) :-
	random(1, 700000, Key),
	( dic_get(Dic, Key, _) ->
	    true
	; dic_lookup(Dic, Key, [a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a])
	),
	N1 is N - 1,
	dict_2(N1, Dic).
