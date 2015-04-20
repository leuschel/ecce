:- module(costmodel_rt, _, [assertions, regtypes]).

:- use_module(library(random)).
:- use_module(library('math/stat')).
:- use_module(library('profiler/profiler_utils')).
:- use_module(library(terms)).
:- use_module(library(lists)).
:- use_module(library('make/system_extra')).


:- multifile costmodel/4.

interval(A,B,N,Data) :-
	interval_(A,B,0,N,Data).

interval_(_A,B,N,N,[B]).
interval_(A,B,I,N,[D|Ds]) :-
	D is (A * (N - I) + B * I ) / N,
	J is I + 1,
	interval_(A,B,J,N,Ds).

uniform(A, B, D) :-
	random(R),
	D is A + (B - A) * R.

gaussian(Mean, StdDev, D) :-
	random(R1),
	random(R2),
	U1 is 2*R1 - 1,
	S2 is U1**2 + (2*R2-1)**2,
	(   S2 < 1 ->
	    D is sqrt(-2*log(S2)/S2) * U1 * StdDev + Mean
	;
	    gaussian(Mean, StdDev, D)
	).

% type_to_value(number, interval(A,B,N), Data) :-
% 	interval(A,B,N,Data).

:- dynamic type_to_value/3.

type_to_value(int, uniform(A,B), Data) :-
	random(A, B, Data).
type_to_value(number, uniform(A,B), Data) :-
	uniform(A,B,Data).
type_to_value(number, gaussian(M,S), Data) :-
	gaussian(M,S,Data).
type_to_value(list(T), list(DistValue, DistLength), Data) :-
	type_to_value(int, DistLength, Length),
	type_to_value_list(T, DistValue, Length, Data).

type_to_value(term, none, _).

type_to_value(cuadmatrix(T), cuadmatrix(DistValue, DistLength), Data) :-
	type_to_value(int, DistLength, Length),
	type_to_value_list2(T, DistValue, Length, Length, Data).

type_to_value_list(_T, _DistValue, 0, []).
type_to_value_list( T,  DistValue, N, [D|Ds]) :-
	type_to_value(T, DistValue, D),
	N2 is N - 1,
	type_to_value_list(T, DistValue, N2, Ds).

:- regtype cuadmatrix(T).
cuadmatrix(Type,X) :-
	list(list(Type),X).

type_to_value_list2(_T, _DistValue, _M, 0, []).
type_to_value_list2( T,  DistValue, M, N, [D|Ds]) :-
	type_to_value_list(T, DistValue, M, D),
	N2 is N - 1,
	type_to_value_list2(T, DistValue, M, N2, Ds).

types_to_values([],[],[]).
types_to_values([T|Ts],[D|Ds],[V|Vs]) :-
	type_to_value(T, D, V),
	types_to_values(Ts, Ds, Vs).

types_to_values_list(_T, _D, 0, []).
types_to_values_list(T, D, N, [V|Vs]) :-
	types_to_values(T, D, V),
	N2 is N - 1,
	types_to_values_list(T, D, N2, Vs).

% :- meta_predicate tests(goal,?,?).

% tests(_Pred, [], []).
% tests(Pred, [Value|Values], [[T|Value]|Ds]) :-
% 	Pred =.. [Functor2|[Pred2]],
% 	Pred2 =.. [Functor|_Value1],
% 	Pred3 =.. [Functor|Value],
% 	Pred4 =.. [Functor2|[Pred3]],
% 	measure(Pred4, T),
% 	tests(Pred, Values, Ds).

tests(_Module, _Functor, _Args, _Params, _GoalCost, [], []).
tests( Module,  Functor,  Args,  Params,  GoalCost, [Value|Values], [[T|Data]|Ds]) :-
	atom_concat([Module, ':', Functor], F),
	Pred =.. [F|Value],
	measure(runtime, '$:'(Pred),T),
	apply_cost_params(Value, Args, Params, GoalCost, Data),
	tests(Module, Functor, Args, Params, GoalCost, Values, Ds).

:- data cost_param/1.

:- meta_predicate apply_cost_params(?,?,?,goal,?).

apply_cost_params(Value, Args, Params, GoalCost, Data) :-
	(
	    Args = Value,
	    call(GoalCost),
	    assertz_fact(cost_param(Params)),
	    fail
	;
	    current_fact(cost_param(Data)),
	    retract_fact(cost_param(Data))
	).

estimate_costmodel(Module, Pred, Dist, model(N, Args, Params, GoalCost, linearmodel(Terms)), Data, B) :-
	Pred =.. [Functor|Types],
	types_to_values_list(Types, Dist, N, Values),
	tests(Module, Functor, Args, Params, GoalCost, Values, Data),
	genregression(Data, Terms, Params, B).

docostmodel(Module, Pred, Dist, Model, Data, B) :-
	costmodel(Module, Pred, Dist, Model),
	estimate_costmodel(Module, Pred, Dist, Model, Data, B).

dumpdataS([], []).
dumpdataS([D|Ds], [L|Ls]) :-
	[Y,X] = D,
	number_codes(X,XS),
	number_codes(Y,YS),
	list_concat([XS, " ", YS, "\n"], L),
	dumpdataS(Ds,Ls).

dumpdata(Data, FileName) :-
	dumpdataS(Data, StringList),
	writef_list(StringList, FileName).

dumpmodelS([], [], []).
dumpmodelS([Coeff|Coeffs], [Func|Funcs], [L|Ls]) :-
	number_codes(Coeff, CoeffS),
	list_concat(["+", CoeffS, "*", Func], L),
	dumpmodelS(Coeffs, Funcs, Ls).

dumpmodel(Coeffs, Funcs, FileName) :-
	dumpmodelS(Coeffs, Funcs, StringList),
	writef_list(StringList, FileName).

%:- costmodel qsort(list(number,N), term) :: list(uniform(-1,1,100),
%   uniform(0,100,100)) * none => model(100, linearmodel([1, N, N *
%   log(N)])).

% estimate_costmodel(mmatrix, mtrans(cuadmatrix(number),term),
% [cuadmatrix(uniform(-10,10),uniform(1,99)),none],
% model(100,[_B,_],[_A],length(_B,_A),linearmodel([1,_A,_A**2])), Data,
% B), dumpdata(Data, 'mmatrix_4.dat').
