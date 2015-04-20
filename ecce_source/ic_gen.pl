:- module(ic_gen,[gen_ic_code/0,gen_padd/0,gen_pdel/0,gen_solve/0,gen_solve_body/3,add_update_arg/3,gen_lts/0,gen_member/0]).

/* --------------------------------------------- */
/* (C) COPYRIGHT MICHAEL LEUSCHEL 1995,1996,1997 */
/* --------------------------------------------- */

/* TO GET THE LATEST VERSION OF THE ECCE SYSTEM GO TO:
	http://www.cs.kuleuven.ac.be/~michael */
/* BUG REPORTS, QUESTIONS AND COMMENTS CAN BE SENT TO:
	michael@cs.kuleuven.ac.be */

:- use_package( .(ecce_no_rt) ).

:- set_prolog_flag(multi_arity_warnings,off).
:- set_prolog_flag(discontiguous_warnings,off).
:- set_prolog_flag(single_var_warnings,off).

:- use_module(library(lists)).

%not(Goal) :- \+(Goal).

:- use_module(bimtools).
:- use_module(code_generator).

/* ---------- */
/* ic-gen.pro */
/* ---------- */

gen_ic_code :-
	reset_spec_prog,
	print('.'),
	gen_padd,
	print('.'),
	gen_pdel,
	print('.'),
	gen_solve,
	print('.'),
	gen_lts,
	print('.'),
	gen_member,
	print('.'),
	clear_database,
	print('.'),
	copy_specialised_program_to_input,
	print('.'),nl,
	print_claus_database_status.


gen_padd :-
	assert_spec_clause(padd(X,Updt),[member(add(X),Updt)]),
	fail.
gen_padd :-
	claus(_Nr,Head,Body),
	add_update_arg(padd(Head),Updt,UHead),
	member(Lit,Body),
	((Lit = not(Atom))
	 -> (add_update_arg(pdel(Atom),Updt,UAtom),
	     assert_spec_clause(UHead,[UAtom])
	    )
	 ;  (add_update_arg(padd(Lit),Updt,ULit),
	     assert_spec_clause(UHead,[ULit])
	    )
	),fail.
gen_padd.

gen_pdel :-
	assert_spec_clause(padd(X,Updt),[member(add(X),Updt)]),
	fail.
gen_pdel :-
	claus(_Nr,Head,Body),
	add_update_arg(pdel(Head),Updt,UHead),
	member(Lit,Body),
	((Lit = not(Atom))
	 -> (add_update_arg(padd(Atom),Updt,UAtom),
	     assert_spec_clause(UHead,[UAtom])
	    )
	 ;  (add_update_arg(pdel(Lit),Updt,ULit),
	     assert_spec_clause(UHead,[ULit])
	    )
	),fail.
gen_pdel.


gen_solve :-
	assert_spec_clause(solve(X,Updt),[member(add(X),Updt)]),
	fail.
gen_solve :-
	claus(_Nr,Head,Body),
	add_update_arg(solve(Head),Updt,UHead),
	gen_solve_body(Body,Updt,SBody),
	assert_spec_clause(UHead,SBody),
	fail.
gen_solve :-
	assert_spec_clause(solve(man(X),U),
		[man(X),not(member(del(man(X)),U))]),
	assert_spec_clause(solve(woman(X),U),
		[woman(X),not(member(del(woman(X)),U))]),
	assert_spec_clause(solve(parent(X,Y),U),
		[parent(X,Y),not(member(del(man(X)),U))]).
gen_solve.

gen_solve_body([],_Updt,[]).
gen_solve_body([not(Atom)|T],Updt,[SAtom|ST]) :- !,
	SAtom = not(solve(Atom,Updt)),
	gen_solve_body(T,Updt,ST).
gen_solve_body([Atom|T],Updt,[solve(Atom,Updt)|ST]) :-
	gen_solve_body(T,Updt,ST).

add_update_arg(Call,Update,CallWithUpdate) :-
	Call =.. [Pred|Args],
	append(Args,[Update],NewArgs),CallWithUpdate =.. [Pred|NewArgs].

gen_lts :-
	claus(_Nr,false(ICNr,Vars),Body),
	gen_solve_body(Body,Updt,SBody),
	PA = padd(false(ICNr,Vars),Updt),
	assert_spec_clause(lts(ICNr,Updt),[PA|SBody]),
	fail.
gen_lts.



gen_member :-
	assert_spec_clause(member(X,[X|_T]),[]),
	assert_spec_clause(member(X,[_Y|T]),[member(X,T)]).
	
