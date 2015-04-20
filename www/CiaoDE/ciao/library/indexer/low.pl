/* @(#)low.pl	1.6 29 Sep 1993

This file supports the files {module_,dynamic_,}indexer.pl. See the
README in this directory for a general discussion on how to use this stuff.

This software is free: please copy it and/or modify it. Do with it what
you like.  It is a gift from KnowledgeWare Inc, and was written by Tom
Howland.

Adapted to Ciao by Francisco Bueno, 8 Jan 2001.
*/

:- module(low, [
    index_clauses/11,
    tidy/2,
    parse_index_specs/15,
    concat_atom/2
   ]).

:- use_module(library(lists),[append/3]).
% :- use_module(library(types)).

% todo:
% - check the role of Mod (module name) in the expansion ...

parse_index_specs(P1, Head, Mod, Md, Arg, Tidy, HAB, HRB, Refs, Dis,
		  Comp, Dyn,
		  [(:- discontiguous '** module initialization **'/2),
		   '** module initialization **'(TheName, TheArity)|MI],
		  TheName, TheArity) :-
    parse_index_specs1(P1, Head, Mod, Md, Arg, 1, Body, HAB, HRB,
		       Refs, Dis, Comp, Dyn, MI),
    tidy(Body, Tidy).

parse_index_specs1((P1, P2), Head, Mod, Md, Arg, N, (B0;B1),
		   (Ab0,Ab1), (Rb0,Rb1), [Rh|Rt], [Dish|Dist], [Comph|Compt],
		   [Dynh|Dynt], [MIh|MIt]):- 
	!,
	parse_index_spec(P1, Head, Mod, Md, Arg, N, B0, Ab0, Rb0, Rh, Dish,
			 Comph, Dynh, MIh),
	M is N + 1,
	parse_index_specs1(P2, Head, Mod, Md, Arg, M, B1, Ab1, Rb1, Rt,
			   Dist, Compt, Dynt, MIt).
parse_index_specs1(P1, Head, Mod, Md, Arg, N, (Body;true), Ab,
		   Rb, [Ref], [Dis], [Comp], [Dyn], [MI]) :-
	parse_index_spec(P1, Head, Mod, Md, Arg, N, Body, Ab, Rb, Ref,
			 Dis, Comp, Dyn, MI).

tidy((A:-B), (A:-X)) :- !, tidy(B, X).
tidy((A;B), (X;Y)) :- !, tidy(A, X), tidy(B, Y).
tidy((A->B), (X->Y)) :- !, tidy(A, X), tidy(B, Y).
tidy((true,X), A) :- !, tidy(X, A).
tidy((X,true), A) :- !, tidy(X, A).
tidy((A,B), (X,Y)) :- !, tidy(A, X), tidy(B,Y).
tidy(X, X).

parse_index_spec(Spec, Head, Mod, Md, Arg, N,
		 (Test -> Tidy, Call),
		 (Tidy,assert(Call,Ref)), erase(Ref), Ref, Dis, Comp,
		 (:- dynamic(IndexName/2)),
		 ('** module initialization **'(IndexName, 2))
		 ) :-
    functor(Head, Name, Arity),
    parse_spec(Arity, Spec, Head, Test, Action, Hashes),
    xor_hashes(Hashes, Hash, LastAct),
    concat_atom(['** ', Name, '/', Arity, ' index ', N, ' **'], IndexName),
    functor(CalltoIndex, IndexName, 2),
    arg(1, CalltoIndex, Hash),
    arg(2, CalltoIndex, Arg),
    (   Md == Mod
    ->  Call = CalltoIndex
    ;   Call = Md:CalltoIndex
    ),
    tidy((Action,LastAct), Tidy),
    Dis = (:-discontiguous(IndexName/2)),

    % the following if-then-else is required for the case when the input
    % clauses do not match the index specs.

    Comp = ('** hash index **'(Head, Mod, Arg, CalltoIndex):-(Test->Tidy;true)).

parse_spec(0, _, _, true, true, []) :-
	!.
parse_spec(N, Spec, Head, (Test0,Test),
		      (Act0, Act), [H|Hashes]) :-
	arg(N, Spec, Arg),
	arg(N, Head, Var),
	hash_spec(Arg, Var, Test0, Act0, H),
	!,
	M is N-1,
	parse_spec(M, Spec, Head, Test, Act, Hashes).
parse_spec(N, Spec, Head, Test, Action, Hashes) :-
	M is N-1,
	parse_spec(M, Spec, Head, Test, Action, Hashes).

hash_spec(+, Var, nonvar(Var), (functor(Var, N, _), hash_term(N, H)), H).
hash_spec(*, Var, ground(Var), hash_term(Var, H), H).
hash_spec(i, Var, integer(Var), true, Var).
hash_spec(n, Var, nonvar(Var), true, Var).

xor_hashes([], _, true) :- !.
xor_hashes([H|T], Left, Expr) :-
   xor_hashes1(T, H, Xors),
   (   var(Xors)
   ->  Expr = true, Left = Xors
   ;   Expr = (Left is Xors)
   ).

%% Was \ instead of # --I guess it was Quintus xor (PBC)
xor_hashes1([], H, H) :- !.
xor_hashes1([H|T], Z, #(Z,X)) :- xor_hashes1(T, H, X).

/* We supply our own version of concat_atom here because Quintus doesn't
   allow dynamic loading of shared objects into runtime systems. The
   runtime system we're talking about here is qpc.
*/
%% In Ciao we can probably do it ...

concat_atom(L, A) :- concat_atom1(L, C), atom_chars(A, C).

concat_atom1([], []).
concat_atom1([H|T], X) :-
    (   atom(H)
    ->  atom_chars(H, C)
    ;   number(H)
    ->  number_chars(H, C)
    ;   C = H
    ),
    append(C, Y, X),
    concat_atom1(T, Y).

index_clauses(Specs, [(TrueHash:-Body)|Rest], Rest, Head, NewHead,
	      NewName, NewArity, FirstArg, Body, Hash, TrueHash) :-
    check_index_specs(Specs, Name, Arity),
    functor(Head, Name, Arity),	% -Head, +Name, +Arity
    concat_atom(['** ', Name, '/', Arity, ' **'], NewName),
    NewArity is Arity + 1,
    functor(NewHead, NewName, NewArity),
    arg(1, NewHead, FirstArg),
    unify_args(NewArity, Head, NewHead),

    concat_atom(['** hash ', Name, '/', Arity, ' **'], HashName),
    functor(Hash, HashName, NewArity),
    fingy(NewArity, NewArity, NewHead, Hash).

fingy(N, M, A, B) :-
    (   N =:= 1
    ->  arg(1, A, X), arg(M, B, X)
    ;   X is N-1, arg(N, A, Z), arg(X, B, Z), fingy(X, M, A, B)
    ).

check_index_specs((Spec,Specs), Name, Arity) :-
	!,
	must_be(callable, Spec, 1, :-(index(Spec, Specs))),
	functor(Spec, Name, Arity),
	check_index_args(Arity, Spec),
	check_index_specs(Specs, Name, Arity).
check_index_specs(Spec, Name, Arity) :-
	must_be(callable, Spec, 1, :-(index(Spec))),
	functor(Spec, Name, Arity).

check_index_args(N, Spec) :-
	( N =:= 0 -> true
	; arg(N, Spec, Arg),
	  check_index_arg(Arg, Spec),
	  N1 is N - 1,
	  check_index_args(N1, Spec)
	).

check_index_arg(Arg, Spec) :-
	must_be(oneof([+,*,-,?,i,n]), Arg, 1, :-(index(Spec))).
	
unify_args(N, T1, T2) :-
        ( N =< 1 ->
                true
        ; N1 is N - 1,
	  arg(N1, T1, Arg),
	  arg(N, T2, Arg),
          unify_args(N1, T1, T2)
        ).
        
must_be(callable, _, _, _). % currently, cannot be checked 
must_be(oneof(List), A, _, Source):-
	( member(A,List) -> true
	; message(warning, ['In ',Source,' argument must be one of ',List,
	                   ': ignored'])
	).
