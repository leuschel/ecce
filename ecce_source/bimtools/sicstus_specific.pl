/* sicstus_specific.pro */

consult_without_redefine_warning(File) :-
    prolog_flag(redefine_warnings, Old, off),
    prolog_flag(single_var_warnings, Old2, off),
    (consult(File)
      -> OK=true ; OK=false),
    prolog_flag(redefine_warnings, _, Old),
    prolog_flag(single_var_warnings, _, Old2),
    OK=true.

:- multifile pre_condition/1, post_condition/1, ecce_type/2.
:- dynamic pre_condition/1, post_condition/1, ecce_type/2.

transform_dcg_term(Term,ExpTerm) :-
	expand_term(Term,ExpTerm). 

ecce_put(X) :- put(X).

max(X,Y,Z) :- Z is max(X,Y).


please(_X,_Y). /* to be improved tw,off tw,on */

rerecord(_X,_Y,_Z) :- print('** rerecord **').

namevars(Term,Z,V,_Name) :- numbervars(Term,Z,V).

hide.

:- use_module(library(terms)).
is_inf(X) :- cyclic_term(X).
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

:- meta_predicate time(:,*).
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

/* From: StdLists.pro */

% length([],0).
% length([_H|T],L) :-
% 	length(T,LT),
% 	L is LT + 1.

% reverse([],L,L).
% reverse([H|T],Acc,Res) :-
% 	reverse(T,[H|Acc],Res).


/* From: instance.pro */


:- if(\+ current_prolog_flag(version_data,sicstus(3,_,_,_,_))).
variant_of(Goal,UIGoal) :-
	copy(Goal,CGoal),
	variant(UIGoal,CGoal).

instance_of(Goal,UIGoal) :- 
	copy(Goal,CGoal),
	subsumes_chk(UIGoal,CGoal).

strict_instance_of(Goal1,Goal2) :-
	copy(Goal1,CGoal),
	subsumes_chk(Goal2,CGoal),
	not(subsumes_chk(CGoal,Goal2)).
:- else.
:- use_module(library(terms),[variant/2]).
variant_of(Goal,UIGoal) :-
	copy(Goal,CGoal),
	variant(UIGoal,CGoal).

instance_of(Goal,UIGoal) :- 
	copy(Goal,CGoal),
	subsumes_term(UIGoal,CGoal).

strict_instance_of(Goal1,Goal2) :-
	copy(Goal1,CGoal),
	subsumes_term(Goal2,CGoal),
	\+(subsumes_term(CGoal,Goal2)).
:- endif.

ecce_get(Ascii) :- get(Ascii).



read_term_with_lines( RTerm , Start , End ) :-
	     read_term(RTerm,[layout(Layout)]), 
	     (Layout=[Start|_],get_end_of_layout(Layout,End)
	      -> true ; (Start = 0,End = -1)).
	     %print(read_term_layout(Start,End,Layout)),nl.


get_end_of_layout(X,X) :- atomic(X),!.
get_end_of_layout([X],R) :- !,get_end_of_layout(X,R).
get_end_of_layout([_|T],R) :- get_end_of_layout(T,R).


retractall_fact( X ) :-
	retractall( X ).
