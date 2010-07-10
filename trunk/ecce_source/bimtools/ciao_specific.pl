
/* ---------------------------------- */
/* (C) COPYRIGHT MAURICIO VAREA, 2003 */
/* ---------------------------------- */

/* ciao_specific.pl */

:- use_module(library(compiler)).

:- use_module(library(terms)).
:- use_module(library(prolog_sys)).
:- use_module(library(terms_check)).
:- use_module(library(system)).

ecce_source_directory('$ECCE_SOURCE/').
ecce_benchmark_directory('$ECCE_BENCHMARKS/').

string_concatenate(X,Y,XY) :-
   name(X,Xs),name(Y,Ys),append(Xs,Ys,XYs),name(XY,XYs).

/* ecce_reconsult(File) :-
	ecce_source_directory(Dir),
	string_concatenate(Dir,File,CF),!,
	ensure_loaded(CF).
ensure_consulted(File) :- 
	ecce_source_directory(Dir),
	string_concatenate(Dir,File,CF),!,
	ensure_loaded(CF). */


transform_dcg_term(Term,ExpTerm) :-
	expand_term(Term,ExpTerm). 


ecce_put(X) :- ground(X), put_code(X).
ecce_put(X) :- var(X), print('non-ground var!!!!').

please(_X,_Y). /* to be improved tw,off tw,on */

rerecord(_X,_Y,_Z) :- print('** rerecord **').

namevars(Term,Z,V,_Name) :- numbervars(Term,Z,V).

%hide.

is_inf(X) :- cyclic_term(X,[]).

cyclic_term(X, Seen) :- seen(X, Seen), !.
cyclic_term(X, Seen) :-
	nonvar(X), X =.. [_|As], member(A, As),
	cyclic_term(A, [X|Seen]).

max(X,Y,X) :- X >= Y.
max(X,Y,Y) :- Y > X.

seen(X, Seen) :- member(X0, Seen), X == X0, !.

/* varlist(T,VList) :- term_variables(T,VList).
 Sicstus version: does not keep variables in order */

varlist(T,VList) :- varlist2(T,[],VList).

varlist2(X,L,R) :- ground(X),!,L=R.
varlist2(X,L,R) :- var(X),!,
   add_var(X,L,R).
varlist2(X,L,R) :- nonvar(X),X=..[_F|Args],!,
  l_varlist2(Args,L,R).
varlist2(_X,L,L) :- print('*** unknown termtype in varlist2'),nl.

add_var(X,[],[X]).
add_var(X,[H|T],Res) :-
  ((X==H) -> (Res = [H|T]) ; (Res = [H|T2], add_var(X,T,T2))).

l_varlist2([],L,L).
l_varlist2([X|T],L,R) :- 
	varlist2(X,L,L2), l_varlist2(T,L2,R).

stop :- halt.

:- meta_predicate time(:,-).
time(Goal,Time) :- 
	statistics(runtime,[Global1,_]),
	call(Goal),
	statistics(runtime,[Global2,_TimeSinceLastStat]),
	Time is Global2 - Global1.

:- meta_predicate time(:).
time(Goal) :-
	time(Goal,Time),
	print('Time for goal: '),print(Goal),
	print(' is: '),print(Time), print(' ms'),nl.

copy(C,CC) :- copy_term(C,CC).

on_exception(_,C,_) :- call(C).

expand_term(X,X). 


/* From: instance.pro */

variant_of(Goal,UIGoal) :- copy(Goal,CGoal),
	\+( \+(( numbervars(CGoal,0,N),
        numbervars(UIGoal,0,N),
	CGoal = UIGoal))).

instance_of(Goal,UIGoal) :- 
	copy(Goal,CGoal),
	ask(CGoal,UIGoal).

strict_instance_of(Goal1,Goal2) :-
	copy(Goal1,CGoal),
	ask(CGoal,Goal2),
	\+ ask(Goal2,CGoal).

/*--------------------*/

ecce_get(Ascii) :- 
	get_byte(Ascii),
	repeat,
	get_byte(10).

real(X) :- float(X).

%%% CIAO does not support CLPFD, so:
project_and_check_constraint(_,_,_).

/* for 'msv_analysis' */
same_length([],[]).
same_length([_|XT],[_|YT]) :- same_length(XT,YT).


read_term_with_lines( RTerm , S , E ) :-
	     read_term(RTerm,[lines(S,E)]).
