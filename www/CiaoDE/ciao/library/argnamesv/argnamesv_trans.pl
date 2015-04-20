:- module(argnamesv_trans, [argnames_def/3, argnames_goal/3, argnames_use/3], [assertions, dcg]).

:- use_module(library(terms), [arg/2]).
:- use_module(library(lists)).
:- use_module(library(aggregates)).

% argnames(Functor, Arity, Str, Module)
:- data argnames/4.

% runtime argnames: emit runtime predicate
% rt_argnames(Functor, Pred, Module)
:- data rt_argnames/3. 

argnames_def(0, _, M) :- !,
        retractall_fact(argnames(_,_,_,M)),
        retractall_fact(rt_argnames(_,_,M)).
argnames_def(end_of_file, Ys, M) :- !,
	findall((Str, ArgGetName), rt_argnames(Str, ArgGetName, M), Xs),
	runtime_info_list(Xs, Ys, [end_of_file]).
argnames_def((:- argnames(Def)), [], M) :- !,
        functor(Def, Str, N),
        ( argnames(Str, _, Def0, M)  ->
            ( Def0 == Def -> true
            ; inform_user(['ERROR: incompatible argnames declaration ',Def])
            )
        ; arg(Def, A), \+ atomic(A) ->
            inform_user(['ERROR: invalid argnames declaration ',Def])
        ; assertz_fact(argnames(Str,N,Def,M))
        ).
argnames_def((:- _), _, _) :- !, fail.
argnames_def((?- _), _, _) :- !, fail.
argnames_def((Head :- Body), (NewHead :- NewBody), Mod) :- !,
        argnames_head(Head, Mod, NewHead, NewBody0, []),
	( NewBody0 = [] ->
	    NewBody = Body
	; list_to_conjs(NewBody0, NewBody1),
	  NewBody = (Body, NewBody1)
	),
	Head \== NewHead.
argnames_def(Head, (NewHead :- NewBody), Mod) :- !,
        argnames_head(Head, Mod, NewHead, NewBody0, []),
	list_to_conjs(NewBody0, NewBody),
	Head \== NewHead.

list_to_conjs([], true) :- !.
list_to_conjs([X], X) :- !.
list_to_conjs([X|Xs], (X, Ys)) :-
	list_to_conjs(Xs, Ys).

argnames_head(Head, Mod, NewHead2, Xs, Ys) :-
	argnames_exp(Head, Mod, NewHead, Xs0, Ys),
	( var(NewHead) ->
	    Head = $(Str, _),
	    argnames(Str, Arity, _, _),
	    functor(Head2, Str, Arity),
	    Xs = [Head2 = NewHead|Xs0],
	    NewHead2 = Head2
	; Xs = Xs0,
	  NewHead2 = NewHead
	).

% BUG: rt_argnames are asserted AFTER end_of_file or 0 (executed by goal expansion)
% So we need this null term expansion
argnames_use($(Str,TheArgs), _, M) :-
        atom(Str),
	argnames(Str, _, _, M),
	argnames_the_args(TheArgs, Args),
	\+ nonvar_args(Args),
	create_rt_info(M, Str, _, _, _, _),
	fail.

% it emits runtime info predicates for each pair in the list

runtime_info_list([]) --> [].
runtime_info_list([X|Xs]) --> runtime_info(X), runtime_info_list(Xs).

% it emits runtime info predicates a (Str, ArgGetName) pair

runtime_info((Str, ArgGetName)) -->
	{ argnames(Str, _, Def, _), functor(Def, _, Arity) },
	runtime_info_2(1, Arity, Def, ArgGetName, Str).

% it emits the relation argname/argnumber

runtime_info_2(I, Arity, _, _, _) -->
	{ I > Arity }, !.
runtime_info_2(I, Arity, Def, ArgGetName, Str) -->
	{ arg(I, Def, ArgName) },
	{ functor(T, Str, Arity) }, 
	{ arg(I, T, A) },
	{ arg_get_head(ArgGetName, ArgName, T, A, ArgGet) },
	[ArgGet], % ArgGetName(ArgName, T, A).
	{ I1 is I + 1 },
	runtime_info_2(I1, Arity, Def, ArgGetName, Str).

% get the runtime info goal and remember to emit the predicate definition

create_rt_info(M, Str, ArgName, T, A, ArgGet) :-
	current_fact(rt_argnames(Str, ArgGetName, M)), !,
	arg_get_head(ArgGetName, ArgName, T, A, ArgGet).
create_rt_info(M, Str, ArgName, T, A, ArgGet) :-
	atom_concat('$argnames_runtime_info_', Str, ArgGetName),
	assertz_fact(rt_argnames(Str, ArgGetName, M)),
	arg_get_head(ArgGetName, ArgName, T, A, ArgGet).

% the head of a argget predicate

arg_get_head(ArgGetName, ArgName, Term, Arg, ArgGet) :-
	functor(ArgGet, ArgGetName, 3),
	arg(1, ArgGet, ArgName),
	arg(2, ArgGet, Term),
	arg(3, ArgGet, Arg).

% expansion of goals

argnames_goal(Goal, NewGoal, Mod) :- !,
        argnames_exp(Goal, Mod, Goal1, NewGoal0, [Goal1]),
	list_to_conjs(NewGoal0, NewGoal),
	NewGoal \== Goal.

% expansion of goal or structure arguments

argnames_args(0, _, _, _) --> !.
argnames_args(N, T0, Mod, T1) --> !,
        { arg(N, T0, A0) },
        { arg(N, T1, A1) },
        { N1 is N-1 },
        argnames_exp(A0, Mod, A1),
        argnames_args(N1, T0, Mod, T1).

argnames_exp(V,_Mod, V) --> { var(V) }, !.
argnames_exp($(Str,TheArgs), Mod, T) -->
	argnames_exp_2(Str, TheArgs, T, Mod), !.
argnames_exp(T0, Mod, T1) -->
        { functor(T0, F, A) },
        { functor(T1, F, A) },
        argnames_args(A, T0, Mod, T1).

argnames_exp_2(Str, TheArgs, T, M) -->
        { atom(Str), TheArgs == argnames, argnames(Str, _, Def, M) }, !,
	{ Def =.. [_|T] }.
argnames_exp_2(Str, TheArgs, T, M) -->
        { atom(Str), nonvar(TheArgs),
	  argnames_the_args(TheArgs, Args),
	  argnames(Str, A, Def, M),
	  functor(T2, Str, A)
	},
	( { nonvar_args(Args) } -> % simply replace the term
	    { T = T2 }
	; [T = T2]
	),
	assign_nonvar_args(Args, Def, A, T2, M),
        % insert a unification and a runtime arg for each nonvar arg
	assign_var_args(Args, A, T, Str, M),
	!.
argnames_exp_2(Str, TheArgs, _, _) -->
        { inform_user(['WARNING: invalid argnames ',Str,' $ ',TheArgs,
	  ' - not translated']),
	  fail
	}.

nonvar_args([]).
nonvar_args('=>'(ArgName, _)) :- nonvar(ArgName).
nonvar_args((A, B)) :- nonvar_args(A), nonvar_args(B).

assign_nonvar_args([], _, _, _, _) --> [].
assign_nonvar_args('=>'(ArgName,A0), Def, N, T, M) -->
        argnames_exp(A0, M, A),
        assign_nonvar_arg(N, ArgName, A, Def, T, M).
assign_nonvar_args((A, B), Def, N, T, M) -->
        assign_nonvar_args(A, Def, N, T, M),
        assign_nonvar_args(B, Def, N, T, M).

% search the argname in def
% CHECK: p${a => 1, a => 2}   how does it behave??????
assign_nonvar_arg(N, ArgName, A, Def, T, M) --> { nonvar(ArgName) }, !,
        { N > 0 }, 
        ( { arg(N, Def, ArgName) } ->
            { arg(N, T, A) }
        ; { N1 is N-1 },
          assign_nonvar_arg(N1, ArgName, A, Def, T, M)
        ).
assign_nonvar_arg(_, _, _, _, _, _) --> [].

assign_var_args([], _, _, _, _) --> [].
assign_var_args('=>'(ArgName,A0), N, T, Str, M) --> 
        argnames_exp(A0, M, A),
        assign_var_arg(N, ArgName, A, T, Str, M).
assign_var_args((A, B), N, T, Str, M) -->
        assign_var_args(A, N, T, Str, M),
        assign_var_args(B, N, T, Str, M).

% insert a runtime argname search
assign_var_arg(_N, ArgName, A, T, Str, M) --> { var(ArgName) }, !,
	{ create_rt_info(M, Str, ArgName, T, A, ArgGet) },
	[ArgGet].
assign_var_arg(_, _, _, _, _, _) --> [].

argnames_the_args({}, []).
argnames_the_args({Args}, Args).

/********************************
  Example translations :

:- argnames person(name, age, profession).

p(person${}).
q(person${age=> 25}).
r(person${name=> D, profession=>prof(D),age=>age(D)}).
s(person${age=>t(25), name=> daniel}).

% argnames(person, 3, person(name,age,profession)).
% 
% p(person(_,_,_)).
% q(person(_,25,_)).
% r(person(A,age(A),prof(A))).
% s(person(daniel,t(25),_)).

********************************/
