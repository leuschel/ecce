:- module(code_generator_isabelle,[print_specialised_program_isa/0,dialog_isa/2]).

:- use_package( .(ecce_no_rt) ).


:- set_prolog_flag(multi_arity_warnings,off).
:- set_prolog_flag(discontiguous_warnings,off).
:- set_prolog_flag(single_var_warnings,off).

:- use_module(library(dec10_io)).
:- use_module(library(sort)).

:- use_module(bimtools).
:- use_module(dynpreds).
:- use_module(code_generator).
:- use_module(calc_chtree).

%:- dynamic spec_clause/3.


/* ---------------- start isabelle stuff ---------------------------- */


/* the location of the last written isabelle theory */

:- dynamic last_isa_file/1.
last_isa_file('~/demo/ecce_isabelle.thy').

/* the location of the last read file containing transition clauses */

:- dynamic last_trans_file/1.
%last_trans_file('~/ecce/ecce_examples/petri-nets/basicME.P').
last_trans_file('~/demo_lopstr03/petri/rtp_inf.pl').

/* the last clause name representing transitions */

:- dynamic last_trans_pred/1.
last_trans_pred(trans).

/* the last clause name representing the initial state */

:- dynamic last_initial_pred/1.
last_initial_pred(start).

/* the last clause names of the specialised original clauses */

:- dynamic last_cover_pred/1.
last_cover_pred([sat__1,sat_eu__2]).

/* global clauses to represent the size of a state vector and the arguments of
   the clause representing the initial state 
*/

:- dynamic last_state_size/1.
:- dynamic last_initial_args/1.

/* some convenient global variables to simplify the printing */

:- dynamic trans_print_started/0.
:- dynamic cover_print_started/0.
:- dynamic trans_print_N/1.

/* -------------------- end isabelle stuff --------------------------- */


/* this calls all other clauses required for printing an isabelle theory,
   includes:
   1. User dialogs
   2. Declaration of new clause database (transitionsystem)
   3. Load transitions
   4. Write theory
   5. Undeclare database (transitionsystem)
*/
 
print_specialised_program_isa :-
        dialog_isa(last_trans_file,'read transitions from file'),
	dialog_isa(last_trans_pred,'predicate describing transitions'),
	dialog_isa(last_initial_pred,'predicate describing initial state'),
	dialog_isa(last_cover_pred,'list of predicates describing coverability (names in unspecialised program)'),
	dialog_isa(last_isa_file,'write theory to file'),
	last_trans_file(TF),
	Data=transitionsystem,
        declare_database(Data),
	see(TF),read_database(Data),seen,
	last_initial_pred(IP), /* only the last argument is considered to be a state */
	claus(Data,_,P,_),P=..[IP|Args],last_el(Args,Arg),assert(last_initial_args(Arg)),
	length_list(Arg,NS),assert(last_state_size(NS)),
	last_isa_file(IF),
	tell(IF),
	print_isa_header,
	print_isa_type_decl,
	print_isa_path_decl(Data),
	print_isa_path_def(Data),
        print_isa_cover_decl,
	print_isa_cover_def,
	print_isa_lemma,
	print_isa_proofscript,
	told,
	clear_database(Data),
	undeclare_database(Data),
	retract(last_initial_args(_)),
	retract(last_state_size(_)).

/* creates generic dialog, prints string S, removes and adds new clause P(T) depending on
   user input T,
   P,S must be instantiated
*/

dialog_isa(P,S):-
	C=..[P,L],C,
	beginner_print('Type a dot (.) and hit return at the end.'),
	beginner_nl,
	print(S),print(' (l for '),print(L),print(') =>'),
	read(N),
	((N=l) -> (T = L);  (T = N,retract(C),CN=..[P,T], assert(CN))).

/* prints theory header */

print_isa_header:-
        print('theory PN = Main:'),nl,nl.

/* prints type declaration of "state" depending on size of state */

print_isa_type_decl:-
	print('types'),nl,
	nl,
	print(' state = "'),
	last_state_size(N),
	print_isa_prod(N,'nat',' \\<times> '),
	print('"'),nl,nl.

/* prints type declarations for predicate "paths", the predicate representing transitions,
   and the predicate representing the initial state(s),
   Data must be instantiated by appropriate clause database
*/

print_isa_path_decl(Data):-
	print('consts'),nl,
	nl,
	print(' paths:: "(state list) set"'),nl,
	nl,
	last_trans_pred(T),
        printtdecl(Data,T),
	nl,
	print(' '),print(T),print(':: "(state \\<times> state \\<times> nat) set"'),nl,nl,
	last_initial_pred(I),last_initial_args(IP),
	collect_vars_isa(IP,V),sort(V,V2),length_list(V2,NI),
	print(' '),print(I),print(' :: "'),print_isa_prod(NI,'nat',' \\<Rightarrow> '),
	(NI \== 0 -> print(' \\<Rightarrow> '); true),print('state"'),nl,nl.

/* prints definitions for "paths", the predicate representing transitions,
   Data must be instantiated by appropriate clause database
*/

print_isa_path_def(Data):-
        print('defs'),nl,
	nl,
	last_trans_pred(T),
	printtdef(Data,T),
	nl,
	last_initial_pred(I),last_initial_args(IP),
	print(' '),print(I),print('_def [simp]: "'),print(I),
        collect_vars_isa(IP,V),sort(V,V2),length_list(V2,NI),
	numbervars(V2,0,_),printvlist_isa(V2), print(' \\<equiv> '),
	print_state_isa(IP),print('"'),nl,
	nl,	
	print(' '),print(T),print('_def: "'), print(T),
	print(' \\<equiv> {(x,y,n). '),
	print_trans_isa(Data,T),
	print('}"'),nl,
	nl,
	print('inductive paths'),nl,
	print('intros'),nl,nl,
	print(' zero: "[('),print(I),(NI \== 0 -> print(' '); true),
	printvlist_isa(V2),print(')] \\<in> paths"'),nl,
	print(' step: "\\<lbrakk> (y,z,n) \\<in> '),print(T),
	print('; y#l \\<in> paths \\<rbrakk> \\<Longrightarrow> z#(y#l) \\<in> paths"'),nl,nl.

/* prints type declarations for predicate "coverrel" and the predicates used to represent the
   coverability relation
*/

print_isa_cover_decl:-
	print('consts'),nl,
	nl,
	print(' coverrel:: "(state \\<times> state) set"'),nl,
	nl,
        last_cover_pred(List),
	member(L,List),
	cg_filter_goal(_NodeID,[Goal|R],_MSVGoal,FGoal),
	Goal=..[L|_],
	numbervars(FGoal,0,NrOfVars),
	N2 is NrOfVars + 26,
	numbervars([Goal|R],N2,_), /* these are redundant arguments */
	print(' '),
        filter_print_atom_name_isa(FGoal),
        print(' :: "'),
	print_isa_prod(NrOfVars,'nat',' \\<Rightarrow> '),
	((NrOfVars > 0)  /* added my mal, August 25 2003 */
        -> print(' \\<Rightarrow> state"')
        ;  print(' state"')
     ),
      nl,
	fail.
print_isa_cover_decl:- nl.

/* prints definitions for the predicates used to represent the coverability relation
*/

print_isa_cover_def:-
	print('defs'),nl,
	nl,
	last_cover_pred(List),
	member(L,List),
	cg_filter_goal(_NodeID,[Goal|R],MsvGoal,FGoal),
	Goal=..[L|_],
	numbervars(FGoal,0,NrOfVars),
	N2 is NrOfVars + 26,
	numbervars([Goal|R],N2,_), /* these are redundant arguments */
	print(' '),
        filter_print_atom_name_isa(FGoal),
        print('_def: "'),
	filter_print_atom_isa(FGoal),
	MsvGoal=[C|_],
	print(' \\<equiv> '),
	C=..[_|A],
	print_state_isa(A),
	print('"'),
        nl,
	fail.

/* prints definition for predicate "coverrel" to represent the coverability relation */

print_isa_cover_def:-
	nl,nl,
	print(' coverrel_def: "coverrel \\<equiv>'),
	spec_clause(SpecClauseNr,FGoal,Body),
	SpecClauseNr\==filter_comment,
        last_cover_pred(List),
	member(L,List),
        cg_filter_goal(_NodeID,[Goal|_],_MsvGoal,FGoal),
        Goal=..[L|_],
        (cover_print_started -> print('\\<union> '); assert(cover_print_started)),
        print_isa_coverel(FGoal,Body),	
	fail.
print_isa_cover_def :-
	cg_filter_goal(_NodeID,_Goal,_MsvGoal,FGoal),
	not(spec_clause(_SpecClauseNr,FGoal,_Body)),
	print_isa_coverel(FGoal,[fail]),
	fail.
print_isa_cover_def:- print('"'),nl,nl,retract(cover_print_started).

/* prints lemma to be verified */

print_isa_lemma :-
	print('lemma "l \\<in> paths \\<Longrightarrow> \\<exists> y. ((hd l),y) \\<in> coverrel"'),nl.

/* prints proofscript */

print_isa_proofscript :-
	print(' apply(erule paths.induct)'),nl,
	print('   apply(simp only: '),
	last_initial_pred(IP),print(IP),print('_def'),nl,
	print('                    coverrel_def)'),nl,
	print('   apply(simp only:'),print_coverpred,
	print(')'),nl,
	print('   apply(simp)'),nl,
	print('   apply(blast)'),nl,
	print('  apply(simp only: '),
	last_trans_pred(TP),print(TP),print('_def)'),nl,
	print('  apply(clarify)'),nl,
	print('  apply(((erule disjE)?,'),nl,
	print('           simp only: coverrel_def,'),nl,
	print('           simp,'),nl,
	print('           ((erule disjE)?,'),nl, 
	print('              simp only:'),print_coverpred,
	print(','),nl,
	print('              simp|blast)+)+)'),nl.


/* slightly modified atom printing for isabelle,
   note: does not produce correct output for Non-atoms
*/

filter_print_atom_isa(':'(Module,Pred)) :- !,
	filter_print_atom_isa(Module),print(':'), /* this is not the proper output for isa */ 
	filter_print_atom_isa(Pred).
filter_print_atom_isa(X) :- X='$VAR'(_),!,
	print_red(X). /* this is not the proper output for isa */ 
filter_print_atom_isa(Atom) :-
	Atom =.. [Pred],!,
	print_faithful_functor(Pred).
filter_print_atom_isa(Atom) :-
	Atom =.. [Pred|Args],
	Args = [_|_],!,
	print_faithful_functor(Pred),
	print(' '),
	l_filter_print_isa(Args).
filter_print_atom_isa(Atom) :- print_atom(Atom).

/* slightly modified atom name printing for isabelle */

filter_print_atom_name_isa(Atom) :-
	Atom =.. [Pred],!,
	print_faithful_functor(Pred).
filter_print_atom_name_isa(Atom) :-
	Atom =.. [Pred|Args],
	Args = [_|_],!,
	print_faithful_functor(Pred).
filter_print_atom_name_isa(Atom) :- print_atom(Atom).

/* slightly modified argument list printing for isabelle */

l_filter_print_isa([H|T]) :-
	filter_print_arg_isa(H),
	l_filter_print1_isa(T).

l_filter_print1_isa([]).
l_filter_print1_isa([H|T]) :-
	print(' '),
	filter_print_arg_isa(H),
	l_filter_print1_isa(T).

/* slightly modified argument printing for isabelle
   note: does not produce correct output for generic variables
*/

filter_print_arg_isa(X) :- var(X),!,
	print_red(X).
filter_print_arg_isa(X) :- X='$VAR'(_),!,
	print_red(X). /* this is not the proper output for isa */ 
filter_print_arg_isa(X) :-
	print_faithful_isa(X). /* print_green */

/* slightly modified argument printing for isabelle
   note: does not produce correct output for generic variables
*/
	
print_faithful_isa(X) :- var(X),!,print_red(X).
print_faithful_isa(X) :- X='$VAR'(_),!, /* this is not the proper output for isa */ 
	print_red(X). /* remove  to faithfully print $VAR variables */
print_faithful_isa(Struct) :-
	Struct =.. [_F],!,
	print_faithful_functor(Struct).
print_faithful_isa([H|T]) :- !,
	print('('),
	print_faithful_isa(H),
	print('#'),
	print_faithful_isa(T),print(')').
print_faithful_isa(Struct) :-
	Struct =.. [s,Arg1], !,
	print('('),
	print('Suc '),
	print_faithful_isa(Arg1),
	print(')').
print_faithful_isa(Struct) :-
	Struct =.. [F,Arg1|Args],
	print('('),
	print_faithful_functor(F),
	print_faithful_isa(Arg1),
	l_print_faithful_isa(Args,63),
	print(')').

/* slightly modified argument list printing for isabelle
   note: does not produce correct output for more than 62 elements
*/

l_print_faithful_isa([],_).
l_print_faithful_isa([H|T],Nr) :-
	print(' '),
	((Nr>1, Nr\==[])
	-> (print_faithful_isa(H), Nr1 is Nr - 1,
	    l_print_faithful_isa(T,Nr1)
	    )
	;  (print('xtra('), /* this is not the proper output for isa */ 
	    print_faithful_isa(H),
	    l_print_faithful_isa(T,63),
	    print(')')
	   )
	).


/* prints a single subset of the coverability relation,
   Head, Body must be instantiated
*/

print_isa_coverel(Head,Body) :-
	collect_vars_isa([Head|Body],V),
	sort(V,V2),
        print('{(x,y). \\<exists> '),
	numbervars(clause(Head,Body),0,_), 
	printvlist_isa(V2), print('. '),
        print('x='),print('('),
	filter_print_atom_isa(Head),print(')'),
	NewBody = Body, /* remove_redundant_calls(Body,NewBody,[]), */
	print_isa_coverel2(NewBody),
	newlinebreak,
	fail.
print_isa_coverel(_Head,_Body).

/* prints a the goal state description of a subset of the coverability relation,
   Arg must be instantiated
*/

print_isa_coverel2([]) :- print('.').
print_isa_coverel2([Call|T]) :-
	print(' \\<and> '),print('y='),print('('),
	print_call_isa(Call),
	print_body1_with_nl_isa(T),print(')'),
	print('}').

/* note that the second clause does not produce isabelle output */

print_body1_with_nl_isa([]).
print_body1_with_nl_isa([Call|T]) :-
	print(', '),nl,
	print('    '),
	print_call(Call),
	print_body1_with_nl_isa(T).

/* prints a predicate or function call, note that negative literals are not supported in
   this version for isabelle
   Call must be instantiated by a call
*/

print_call_isa(Call) :-
	(is_negative_literal(Call,NegatedAtom) 
	-> (print_bold('\\+('),print_atom(NegatedAtom),print_bold(')'))/* this does not print properly for isa */
	;  ((nonvar(Call),infix_predicate(Call),Call =.. [Pred,Arg1,Arg2])
	    -> (filter_print_atom_isa(Arg1),print(' '),print(Pred),print(' '),
		filter_print_atom_isa(Arg2))
	    ; filter_print_atom_isa(Call)
	   )
	).


/* filters the list of Arg1 to obtain only the variables in Arg2
   Arg1 must be instantiated by a list
*/

collect_vars_isa([],[]).
collect_vars_isa([H|T],[H|L]) :- var(H), collect_vars_isa(T,L).
collect_vars_isa([H|T],L) :- nonvar(H), H =.. [_|N],
                                  collect_vars_isa(N,L1),
                                  collect_vars_isa(T,L2),
				  appendl_isa(L1,L2,L).


/* append two lists
   Arg1, Arg2 must be instantiated by lists
*/

appendl_isa([],L2,L2).
appendl_isa([H|T],L2,[H|L]) :- appendl_isa(T,L2,L).

/* prints a list as space seperated sequence
   Arg must be a list
*/

printvlist_isa([]).
printvlist_isa([H]) :- print(' '), print(H).
printvlist_isa([H1|[H2|T]]) :- print(' '), print(H1), printvlist_isa([H2|T]).

/* prints sequence (ES)^(N-1)E
   N,E,S must be instantiated, N to a number
*/

print_isa_prod(0,_,_).
print_isa_prod(1,E,_) :- print(E).
print_isa_prod(N,E,S) :- N>1, M is N-1,
	           print(E), print(S),
                   print_isa_prod(M,E,S).

/* calculates length of list if Arg1 is instantiated,
   generates list of length Arg2 if Arg1 is uninstantiated
*/

length_list([],0).
length_list([_|T],N) :- (ground(N)-> (N>0,M is N-1);true),
	                 length_list(T,M), N is M+1.

/* picks the last element of a list
   Arg1 must be instantiated by list
*/


last_el([],[]).
last_el([A],A).
last_el([H|[H1|T]],A) :- last_el([H1|T],A).

/* prints a state vector, including variables and conversion of function
   s(X) into (Suc X)
   X must be instantiated to list
*/

print_state_isa(X) :- print('('), print_state_isa1(X), print(')').
print_state_isa1([]). 
print_state_isa1([H]) :- print_state_isa2(H).
print_state_isa1([H1|[H2|T]]) :-
	print_state_isa2(H1), print(','),
	print_state_isa1([H2|T]).
print_state_isa2(X) :- var(X), print(X).
print_state_isa2(X) :- atomic(X), print(X).
print_state_isa2(X) :- X='$VAR'(_),!,print(X).
print_state_isa2(S) :-
	nonvar(S),
	S =.. [s,Arg1], !,
	print('('),
	print('Suc '),
	print_state_isa2(Arg1),
	print(')').

/* print transition relation
   Data must be instantiated to clause database, T to clause name
*/

print_trans_isa(Data,T) :-
	TH=..[T,N,S1,S2],
	claus(Data,_,TH,_),
	(trans_print_started -> print('\\<or> '); assert(trans_print_started)),
	appendl_isa(S1,S2,S),
	collect_vars_isa(S,V),
	sort(V,V2),
        print('(\\<exists> '),
	numbervars(TH,0,_), 
	printvlist_isa(V2), print('. '),
        print('(x,y,n)=('),
	print_state_isa(S1),print(','),
	print_state_isa(S2),print(','),
	print(N),
	print('))'),
	nl,
	fail.
print_trans_isa(_,_) :- retract(trans_print_started).

/* print transition number definition
   Data must be instantiated to clause database, T to clause name
*/

printtdef(Data,T) :-
	TH=..[T,N,_,_],
	claus(Data,_,TH,_),
	(trans_print_N(Num) ->
	    (retract(trans_print_N(Num)),M is Num+1,assert(trans_print_N(M)));
	    M is 0, assert(trans_print_N(M))),
        print(' '),print(N),print('_def [simp]: "'), print(N),print(' \\<equiv> '),
	print(M),print('"'),nl,
	fail.
printtdef(_,_) :- retract(trans_print_N(_)).

/* print transition name declaration
   Data must be instantiated to clause database, T to clause name
*/

printtdecl(Data,T) :-
	TH=..[T,N,_,_],
	claus(Data,_,TH,_),
        print(' '),print(N),print(' :: "nat"'),nl,
	fail.
printtdecl(_,_).

/* print space seperated list of cover predicate definition names */

print_coverpred:-
	last_cover_pred(List),
	member(L,List),
	cg_filter_goal(_NodeID,[Goal|_],_MsvGoal,FGoal),
	Goal=..[L|_],
	print(' '),filter_print_atom_name_isa(FGoal),print('_def'),
	fail.
print_coverpred.

