:- module( 'selectionrule.common' , _ ).


:- use_module( '../calc_chtree' ).

:- use_module( '../static_dynamic_functors.pl' ).

:- use_module( '../homeomorphic.pl' ).

:- use_package( .('../ecce_no_rt2') ).

:- set_prolog_flag(single_var_warnings,off).

get_sel_literal([H|T],H,1,UnfHist).
get_sel_literal([H|T],Sel,Nr,UnfHist) :-
	((UnfHist = []) ; (is_built_in_literal(H), H \== call(_) )),
	get_sel_literal(T,Sel,TNr,UnfHist),
	Nr is TNr + 1.



may_loop(T,S) :-
	may_loop2(T,S,2). /* max_depth = 2 */

may_loop2(T,S,Depth) :-
	stop_may_loop(T,S,Depth),!,
	mixtus_term_size(T,TS),
	mixtus_term_size(S,SS),
	TS =< SS.
may_loop2(T,S,Depth) :-
	nonvar(T),
	nonvar(S),
	atoms_have_same_predicate(T,S,Predicate),
	D1 is Depth - 1,
	T =.. [Pred|TA],
	S =.. [Pred|TS],
	all_may_loop2(TA,TS,D1).

stop_may_loop(T,S,Depth) :- Depth < 1.
stop_may_loop(T,S,Depth) :- var(S),!.
stop_may_loop(T,S,Depth) :- dynamic_term(S),not(number(S)).
stop_may_loop(T,S,Depth) :- inf_number(S).

inf_number(T) :-
	number(T),
	AT is abs(T),
	AT >= 7. /* maxfinite = 7 */

all_may_loop2([],[],D).
all_may_loop2([H|T],[SH|ST],D) :-
	may_loop2(H,SH,D),
	all_may_loop2(T,ST,D).
