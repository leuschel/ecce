:- module(argnamesvv_trans, [argnames_def/3, argnames_use/3], [assertions, dcg]).

:- use_module(library(terms), [arg/2]).
:- use_module(library(lists)).
:- use_module(library(aggregates)).

% argnames_str(StrN, StrA, Module)
:- data argnames_str/3.
% argnames_method(Method, Str, Module)
:- data argnames_method/3.

argnames_def(0, _, M) :- !,
        retractall_fact(argnames_str(_, _, M)),
        retractall_fact(argnames_method(_, _, M)).
argnames_def(end_of_file, Ys, Mod) :- !,
	runtime_info_list(Mod, Ys, [end_of_file]),
	display(Ys), nl.
argnames_def((:- functor_class(Str)), [], Mod) :- !,
        register_str(Str, Mod).
argnames_def((:- _), _, _) :- !, fail.
argnames_def((?- _), _, _) :- !, fail.
argnames_def((Head0 :- Body), (Head :- Body), Mod) :- !,
	Head0 = $(This, StrN):Method,
	nonvar(StrN), nonvar(Method),
	argnames_str(StrN, StrA, Mod),
	n2_head(This, StrN, StrA, Method, Head),
	register_method(StrN/StrA, Method, Mod).
argnames_def(Head, Sentence, Mod) :- !,
	Head = $(_, _):_,
        argnames_def((Head :- true), Sentence, Mod).
	
register_method(Str, Method, Mod) :-
	current_fact(argnames_method(Method, Str, Mod)), !.
register_method(Str, Method, Mod) :-
	assertz_fact(argnames_method(Method, Str, Mod)).

register_str(StrN/StrA, Mod) :-
	current_fact(argnames_str(StrN, StrA, Mod)), !.
register_str(StrN/StrA, Mod) :-
	assertz_fact(argnames_str(StrN, StrA, Mod)).

n0_head(This, StrN, Method, Head) :-	
	Name = '$argnames_n0',
	Head =.. [Name, This, StrN, Method].

n1_head(This, StrN, StrA, Method, Head) :-
	str_atom(StrN/StrA, StrAtom),
	atom_concat('$argnames_n1_', StrAtom, Name),
	Head =.. [Name, Method, This].

n2_head(This, StrN, StrA, Method, Head) :-
	Method =.. [MethodName|Args0],
	append(Args0, [This], Args),
	str_atom(StrN/StrA, StrAtom),
	atom_concat('$argnames_n2_', StrAtom, Name1),
	atom_concat(Name1, '_', Name2),
	atom_concat(Name2, MethodName, Name),
	Head =.. [Name|Args].

str_atom(N/A, Atom) :-
	number_codes(A, ACodes),
	atom_codes(AAtom, ACodes),
	atom_concat(N, '_', N2),
	atom_concat(N2, AAtom, Atom).

argnames_use($(This, StrN):Method, Goal, _Mod) :-
	n0_head(This, StrN, Method, Goal).

% it emits runtime info predicates for each pair in the list
runtime_info_list(Mod) -->
	{ findall(StrN/StrA, argnames_str(StrN, StrA, Mod), Xs) },
	{ display(Xs), nl },
	runtime_info_list_n1(Xs),
	runtime_info_list_n2(Xs, Mod).

runtime_info_list_n1([]) --> [].
runtime_info_list_n1([StrN/StrA|Xs]) -->
	{ n0_head(This, StrN, Method, N0Head) },
	{ n1_head(This, StrN, StrA, Method, N1Head) },
	{ functor(This2, StrN, StrA) },
	[(N0Head :- This = This2, N1Head)],
	runtime_info_list_n1(Xs).

runtime_info_list_n2([], _) --> [].
runtime_info_list_n2([Str|Xs], Mod) -->
	{ findall((Str, Method), argnames_method(Method, Str, Mod), Ys) },
	runtime_info_list_n2_2(Ys, Mod),
	runtime_info_list_n2(Xs, Mod).

runtime_info_list_n2_2([], _) --> [].
runtime_info_list_n2_2([(StrN/StrA, Method)|Xs], Mod) -->
	{ n1_head(This, StrN, StrA, Method, N1Head) },
	{ n2_head(This, StrN, StrA, Method, N2Head) },
	[(N1Head :- N2Head)],
	runtime_info_list_n2_2(Xs, Mod).
