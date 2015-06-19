
/* --------------------------------------------- */
/* (C) COPYRIGHT MICHAEL LEUSCHEL 1995,1996,1997 */
/* --------------------------------------------- */

/* ------------------------ */
/* TYPE CHECKER FOR PROLOG  */
/* ------------------------ */

%:-use_module(calc_chtree).

/* file: typechecker.pro */


/*
=============================================================================
Using the type checker:
=============================================================================

1) Built-in types:

The following are built-in types:
	any
	ground
	nonground
	var
	nonvar
	integer
	real
	number
	atom
	atomic
	ask(Type) [ask user whether of type]

The following are built-in type constructors:
	list/1  -> list(TypeOfListElements)
	vlist/1 -> same as list/1 except that variables also match this type
	term/2  -> term(Functor,ListOfTypesForArguments)
	vterm/2 -> same as term/2 except that variables also match this type

=============================================================================

2) Defining your own types:

just enter ecce_type/2 definitions for each type you want to use, like:

	ecce_type(program,list(clause)).
	ecce_type(clause,term(cl,[atom,list(atom)])).
	ecce_type(atom,any).

You can also define your own type constructors:

	ecce_type(binary_tree(Type),term(null,[])).
	ecce_type(binary_tree(Type),
		term(tree,[binary_tree(Type),Type,binary_tree(Type)])).

=============================================================================

3) Type checking:

Just call:
	term_is_of_type([1,2,3],list(integer)).
	term_is_of_type(tree(null,1,null),binary_tree(ground)).

You can also call
	check_for_illegal_types.
to test whether you have introduced some illegal type definitions
 (it might currently loop for badly defined types like "type(rec,rec)." ).
=============================================================================
*/


/* ------------------------- */
/* check_for_illegal_types/0 */
/* ------------------------- */

/* call once at the beginning to check for illegal type definitions */

check_for_illegal_types :-
	ecce_type(Type,Descr),
	predefined_type(Type),
	print_type_definition_error(any,Descr).
check_for_illegal_types :-
	ecce_type(_Type,Descr),
	\+(legal_type_description(Descr)),
	print_type_definition_error(any,Descr).

print_type_definition_error(Type,Descr) :-
	print('### Error trying to redefine built-in type: '),
	print(Type),nl,
	print('### Description: '),print(Descr),nl.


/* ----------------- */
/* predefined_type/1 */
/* ----------------- */

/* checks whether something is a predefined type */

predefined_type(any).
predefined_type(ground).
predefined_type(nonground).
predefined_type(var).
predefined_type(nonvar).
predefined_type(integer).
predefined_type(real).
predefined_type(number).
predefined_type(atom).
predefined_type(atomic).
predefined_type(list(_X)).
predefined_type(term(_F,_TL)).
predefined_type(vlist(_X)).
predefined_type(vterm(_F,_TL)).
predefined_type(ask(_T)).


/* ------------------------ */
/* legal_type_description/1 */
/* ------------------------ */

/* checks whether something is a legal type description */

legal_type_description(X) :- var(X),!,fail.
legal_type_description(any).
legal_type_description(ground).
legal_type_description(nonground).
legal_type_description(var).
legal_type_description(nonvar).
legal_type_description(integer).
legal_type_description(real).
legal_type_description(number).
legal_type_description(atom).
legal_type_description(atomic).
legal_type_description(ask(_Type)).
legal_type_description(list(Type)) :-
	legal_type_description(Type).
legal_type_description(vlist(Type)) :-
	legal_type_description(Type).
legal_type_description(term(_Functor,TypeList)) :-
	l_legal_type_description(TypeList).
legal_type_description(vterm(_Functor,TypeList)) :-
	l_legal_type_description(TypeList).
legal_type_description(Type) :-
	\+(predefined_type(Type)),
	ecce_type(Type,Descr),
	legal_type_description(Descr).

l_legal_type_description([]).
l_legal_type_description([Type1|Rest]) :-
	legal_type_description(Type1),
	l_legal_type_description(Rest).


/* ----------------- */
/* term_is_of_type/2 */
/* ----------------- */

term_is_of_type(Term,Type) :-
	term_is_of_type(Term,Type,yes). /* yes for print error messages */


/* ----------------- */
/* term_is_of_type/3 */
/* ----------------- */

/* if the third argument is yes then an error message is printed if the
	type check does not succeed and the call to term_is_of_type
	will NEVER FAIL !! */
/* if the third argument is different from yes than no message will be printed
	but the call will fail if the type check fails */

/* term_is_of_type(Term,Descr,PE) :-
	print(tiot(Term,Descr)),nl,fail. */

term_is_of_type(Term,Descr,PrintErrMsg) :-
	is_inf(Term),!,
	print_inf_error(Term,Descr,PrintErrMsg),
	read(_Cont).


%term_is_of_type(_,_,_) :- !.
%term_is_of_type(Term,literal,_PrintErrMsg) :-
%	nonvar(Term),!.
term_is_of_type(_Term,any,_PrintErrMsg) :-
	!. /* anything is of type any */
term_is_of_type(Term,ground,_PrintErrMsg) :-
	ground(Term),!.
term_is_of_type(Term,nonground,_PrintErrMsg) :-
	\+(ground(Term)),!.
term_is_of_type(Term,var,_PrintErrMsg) :-
	var(Term),!.
term_is_of_type(Term,nonvar,_PrintErrMsg) :-
	nonvar(Term),!.
term_is_of_type(Term,integer,_PrintErrMsg) :-
	integer(Term),!.
term_is_of_type(Term,real,_PrintErrMsg) :-
	real(Term),!.
term_is_of_type(Term,number,_PrintErrMsg) :-
	number(Term),!.
term_is_of_type(Term,atom,_PrintErrMsg) :-
	atom(Term),!.
term_is_of_type(Term,atomic,_PrintErrMsg) :-
	atomic(Term),!.
term_is_of_type(Term,ask(Type),_PrintErrMsg) :-
	!,
	print('Type => '),print(Type),nl,
	print('Term => '),print(Term),nl,
	print('(y or n) =>'),read(UserChoice),
	UserChoice=y.
term_is_of_type(Term,list(Type),PrintErrMsg) :-
	var(Term),!,
	print_type_error(Term,list(Type),PrintErrMsg).
term_is_of_type([],list(_Type),_PrintErrMsg) :- !.
term_is_of_type([Head|Tail],list(Type),PrintErrMsg) :-
	term_is_of_type(Head,Type,PrintErrMsg),!,
	term_is_of_type(Tail,list(Type),PrintErrMsg).
term_is_of_type(Term,vlist(_Type),_PrintErrMsg) :-
	var(Term),!. /* also to avoid modifying Term in the next 2 clauses */
term_is_of_type([],vlist(_Type),_PrintErrMsg) :- !.
term_is_of_type([Head|Tail],vlist(Type),PrintErrMsg) :-
	term_is_of_type(Head,Type,PrintErrMsg),!,
	term_is_of_type(Tail,vlist(Type),PrintErrMsg).
term_is_of_type(Term,term(Functor,ArgTypeList),PrintErrMsg) :-
	var(Term),!,
	print_type_error(Term,term(Functor,[ArgTypeList]),PrintErrMsg).
term_is_of_type(Term,term(Functor,ArgTypeList),PrintErrMsg) :-
	Term =.. [Functor|Args],!,
	l_term_is_of_type(Args,ArgTypeList,PrintErrMsg), !.
term_is_of_type(Term,vterm(_Functor,_ArgTypeList),_PrintErrMsg) :-
	var(Term),!. /* also to avoid modifying Term in the next clause */
term_is_of_type(Term,vterm(Functor,ArgTypeList),PrintErrMsg) :-
	Term =.. [Functor|Args],!,
	l_term_is_of_type(Args,ArgTypeList,PrintErrMsg), !.
term_is_of_type(Term,Type,_PrintErrMsg) :-
	\+(predefined_type(Type)),
	ecce_type(Type,Descr), /* user defined type */
	term_is_of_type(Term,Descr,no),!.
	/* printing=no because several alternative options might exist */

term_is_of_type(Term,Type,PrintErrMsg) :-
	print_type_error(Term,Type,PrintErrMsg).


l_term_is_of_type([],[],_PrintErrMsg).
l_term_is_of_type([Term1|Rest],[Type1|TypeList],PrintErrMsg) :-
	term_is_of_type(Term1,Type1,PrintErrMsg),!,
	l_term_is_of_type(Rest,TypeList,PrintErrMsg).

print_type_error(Term,Type,yes) :- nl,
	print('### Type Error: Not '),print(Type),nl,
	print('###      Term : '),safe_print_term(Term),nl,
	assert_type_error.


print_inf_error(Term,Type,yes) :- nl,
	print('### Cyclic Term Error: Requested Type '),print(Type),nl,
	print('### Term: '),safe_print_term(Term),nl,
	assert_type_error.

safe_print_term(X) :- var(X),!,print(X).
safe_print_term(X) :- is_inf(X),!,
	X =.. [Func|Args],
	print(Func),print('('),
	l_safe_print(Args),
	print(')').
safe_print_term(X) :- print(X).

l_safe_print([]).
l_safe_print([Term|T]) :-
	(is_inf(Term)
	 -> (print('!CYCLIC!'))
	 ;  (print(Term))
	), 
	((T=[]) -> true ; print(',') ),
	l_safe_print(T).

:- dynamic type_error/1.
type_error(no).

assert_type_error :- 
	retract(type_error(_N)),fail.
assert_type_error :-
	assert(type_error(yes)).


reset_type_error :-
	retract(type_error(_N)),fail.
reset_type_error :-
	assert(type_error(no)).

type_error_occurred :-
	type_error(yes).
