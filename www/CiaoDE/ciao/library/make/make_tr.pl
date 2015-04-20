:- module(make_tr, [generic_dot_concat/2,defdep/3,concat_bodies/3],
	[assertions, 'make/make_ops',assertions,debugpred]).

:- use_module(library(messages), [error_message/2]).
:- use_module(library('make/make_rt'),[dot_concat/2]).

:- debugpredstatus(off).
%:- debugpredstatus(on).

:- debugsentence((:- use_module(library(write)))).

:- debugpred debug_transformation/1.
:- debugpred write_clause/1.
:- debugpred write_clauses/1.

%% defdep( ( T <= S :: F <- Deps :- Body ), Clauses, _Mod) :- 
%% 	!,
%% 	Clauses = [ (  do_dependency(T,S,F)         :- Body ), 
%% Note that this is different from what lpdoc 2.0 is using now!
%% 	            (  dependency_precond(T,S,Deps) :- true ), 
%% 	            (  dependency_exists(T,S)       :- true ) ], 
%% 	debug_transformation(Clauses).

%% %% Example (should call make(precond):
%% double <= simple <- precond :: Name :-
%% 	readf(~atom_concat([Name,'.simple']),Content),
%% 	append(Content,[0'\n|Content],DoubleContent),
%% 	writef(DoubleContent,~atom_concat([Name,'.double'])).

:- data dependency_exists/3.
:- data target_exists/2.
%:- data dependency_separator/2.

% generic_concat(A, B, C) :-
% 	atom(A), atom(B) ->
% 	atom_concat(A,B,C)
%         ;
% 	(   A=='' ->
% 	    C = B
% 	;   (   B=='' ->
% 		C = A
% 	    ;	C = ~(atom_concat(A,B))
% 	    )
% 	).

% dot_concat(A, A) :-
% 	A == '',
% 	!.
% dot_concat(A, A) :-
% 	atom_codes( A , [ 0'. |_]),
% 	!.
% dot_concat(T, TSuffix) :-
% 	atom(T),
% 	!,
% 	atom_concat('.', T, TSuffix).
%dot_concat(A, A).

generic_dot_concat(T, TSuffix) :-
	atom(T),
	!,
	dot_concat(T, TSuffix).
generic_dot_concat(T, TSuffix) :-
	TSuffix = ~(dot_concat(T)).

% by default the separator is '.' but could be ''
% defdep(0,_,M) :-
% 	asserta_fact(dependency_separator('.',M)).
defdep(end_of_file, end_of_file, M) :-
	retractall_fact(dependency_exists(_,_,M)),
	retractall_fact(target_exists(_,M)).
%	retractall_fact(dependency_separator(_,M)).
% The next is the syntax to put a dependency_precond/3:
defdep( ( T <= S <- F :- _Body ), [], _Mod) :-
	!,
%	dependency_separator(Separator,Mod),
	error_message("Option ~w <= ~w <- ~w :- Body " ||
	    "is no longer supported. Wse T <= S <- D :: F :- B instead",
	    [T, S, F]).
%	generic_dot_concat(T,TSuffix),
%	generic_dot_concat(S,SSuffix),
%	Clauses = [ ( dependency_precond(TSuffix,SSuffix,F) :- Body ) ],
%	debug_transformation(Clauses).

defdep( ( T <= S <- Deps :: F :- Body ), Clauses, Mod) :-
	!,
%	dependency_separator(Separator,Mod),
	generic_dot_concat(T,TSuffix),
	generic_dot_concat(S,SSuffix),
	Clauses0 = [ ( do_dependency(TSuffix,SSuffix,F) :- Body ) ],
	(   dependency_exists(TSuffix, SSuffix, Mod) ->
	    Clauses1 = Clauses0
	;
	    Clauses1 = [ ( dependency_exists(TSuffix, SSuffix) :- 
			 true )|Clauses0],
	    assertz_fact(dependency_exists(TSuffix, SSuffix, Mod))
	),
	(   Deps == [] ->
	    Clauses = Clauses1
	;
	    Clauses = [ ( dependency_precond(TSuffix,SSuffix,Deps) :-
			true )|Clauses1]
	),
	debug_transformation(Clauses).
defdep( ( T <= S :: F :- Body ), Clauses, Mod) :-
	!,
	defdep( ( T <= S <- [] :: F :- Body ), Clauses, Mod).
% 	!,
% 	Clauses =  [ ( do_dependency(T,S,F) :- Body ),
% 		     ( dependency_exists(T,S) :- true ) ],
% 	debug_transformation(Clauses).
defdep( ( Target <- Deps # Comment :- Body ), Clauses, Mod) :-
	!,
	Clauses = [ ( target_comment(Target, Comment, []) :- true )|Clauses0],
	defdep( ( Target <- Deps :- Body ), Clauses0, Mod).
defdep( ( Target <- Deps :- Body ), Clauses, Mod) :-
	!,
	Clauses0 = [ ( do_target(Target) :- Body ) ],
	(   target_exists(Target, Mod) ->
	    Clauses1 = Clauses0
	;
	    Clauses1 = [ ( target_exists(Target) :- true )|Clauses0],
	    assertz_fact(target_exists(Target, Mod))
	),
	(   Deps == [] ->
	    Clauses = Clauses1
	;
	    Clauses = [ ( target_deps(Target,Deps) :- true )|Clauses1]
	),
	debug_transformation(Clauses).
defdep( ( Target <- :- Body ), Clauses, Mod) :-
	!,
	defdep( ( Target <- [] :- Body ), Clauses, Mod).
% 	Clauses = [ (  do_target(Target)        :- Body  ),
% 	            (  target_exists(Target)    :- true  )],
% 	debug_transformation(Clauses).

% This clause is for backward compatibility:
defdep( ( Target <- Deps # Comment :: TargetName :- Body ), Clauses, Mod) :-
	nonvar(Comment),
	!,
	defdep( ( Target <- Deps :: TargetName # Comment :- Body ),
	Clauses, Mod).

defdep( ( Target <- Deps :: TargetName # Comment :- Body ), Clauses, Mod) :-
	nonvar(Comment),
	!,
	Clauses = [ ( target_comment(Target, Comment, []) :- true )|Clauses0],
	defdep( ( Target <- Deps :: TargetName :- Body ), Clauses0, Mod).
defdep( ( Target <- Deps :: TargetName :- Body ), Clauses, Mod) :-
	!,
	% Note that TargetName must be solved at run time
	(
	    atom(Target) ->
	    Clauses0 = [ ( do_target(Target) :- TargetName=Target, Body ) ]
	;   Clauses0 = [ ( do_target(TargetName) :- TargetName=Target, Body ) ]
	),
	(   target_exists(Target, Mod) ->
	    Clauses1 = Clauses0
	;
	    Clauses1 = [ ( target_exists(Target) :- true )|Clauses0],
	    assertz_fact(target_exists(Target, Mod))
	),
	(   Deps == [] ->
	    Clauses = Clauses1
	;
	    Clauses = [ ( target_deps(TargetName,Deps) :-
			TargetName=Target )|Clauses1]
	),
	debug_transformation(Clauses).
%	defdep( ( Target <- Deps :- Body ), Clauses, Mod).
defdep( (:- target_comment(Target, Comment)),
	[ ( target_comment(Target, Comment, []) ) ], _Mod).
defdep( (:- target_comment(Target, Comment, Args)),
	[ ( target_comment(Target, Comment, Args) ) ], _Mod).
% defdep( (:- dependency_separator(Separator) ), [], Mod) :-
% 	retractall_fact(dependency_separator(_, Mod)),
% 	asserta_fact(dependency_separator(Separator, Mod)).

concat_bodies((G, Gs), B, (G, NB)) :- !,
        concat_bodies(Gs, B, NB).
concat_bodies(G, B, (G, B)).

% --- debugging ---

% debug_transformation(_Clauses) :-
% 	debug_tr(off),
% 	!.
debug_transformation(Clauses) :-
% 	debug_tr(on),
% 	!,
	prolog_flag(write_strings,Old,on),
	write('*** New set of clauses: '), nl,
	write_clauses(Clauses),
	nl,
	prolog_flag(write_strings,_,Old).

write_clauses([]) :-
	!.
write_clauses([Clause|Clauses]) :-
	!,
	write_clause(Clause),
	write_clauses(Clauses).
write_clauses(Clause) :- 
	write('*** Strange format found ***:'), nl,
	write_clause(Clause).

write_clause(Clause) :-
	write(Clause),write('.'),nl.
