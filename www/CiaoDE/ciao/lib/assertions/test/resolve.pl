:- use_module(library(lists),[append/3]).
:- use_module(library(messages)).
:- use_module(library('vndict'),[complete_dict/3,varnamesl2dict/2]).

resolve_applications([],[],_S,_LB,_LE) :- 
	!.
%% newer ciao versions translate T(X,Y) to call(T,X,Y) instead.
%% resolve_applications([apply(CF,[Arg])|R],[Prop|NR]) :-
%% 	!,
%% 	CF =.. [PF|FArgs],
%% 	Prop =.. [PF,Arg|FArgs],
%% 	resolve_applications(R,NR).
resolve_applications([Call|R],[Prop|NR],S,LB,LE) :-
	nonvar(Call),
	Call =.. [call,CF|Args],
	!,
	(  nonvar(CF)
	-> CF =.. [PF|FArgs],
	   %% we take care of call(foo(X),Y)
	   append(FArgs,Args,AllArgs), 
	   %% we take care recursively of nesting: call(foo,X,call(bar,Y))
	   resolve_applications(AllArgs,AllArgsResolved,S,LB,LE),
	   Prop =.. [PF|AllArgsResolved]
	;  error_message(loc(S,LB,LE),
	   "principal functor not sufficiently instantiated in mode: ~w",
                         [Call]),
	   fail
        ),
	resolve_applications(R,NR,S,LB,LE).
resolve_applications([Prop|R],[Prop|NR],S,LB,LE) :-
	resolve_applications(R,NR,S,LB,LE).

bind_dict_varnames([]).
bind_dict_varnames([VarName=Var|Rest]) :-
	VarName=Var,
	bind_dict_varnames(Rest).
