
/* --------------------------------------------- */
/* (C) COPYRIGHT MICHAEL LEUSCHEL 1995,1996,1997 */
/* --------------------------------------------- */


/* New version by Helko Lehmann 2002 */
/* extended to allow the use of multiple clause databases simultaneously */

/* ------------------- */
/* DATABASE OF CLAUSES */
/* ------------------- */
/* file: claus_database.pro */

% FOR CIAO!
:- use_module('bimtools/makeflat').
:- use_module('bimtools/makeiff').
%:- use_module('../bimtools.pl').

% FOR SICSTUS!
%:- use_module(makeflat).
%:- use_module(makeiff).

:- use_module(library(lists)).

/* ------------------------------------------ */
/* REPRESENTING THE PROGRAM TO BE SPECIALISED */
/* ------------------------------------------ */

/* new predicate to store database names */


:- dynamic declared_database/1.
declared_database(compat).

:- dynamic claus/4.
:- dynamic next_free_clause_nr/2.

:- dynamic mode_declaration/6. 
	/* mode(Nr,p(In,Any,Out),[In],[Any],[Out]). */
:- dynamic next_free_mode_nr/2.

next_free_clause_nr(compat,N) :- next_free_clause_nr(N).
next_free_mode_nr(compat,N) :- next_free_mode_nr(N).

claus(compat,A,B,C) :- claus(A,B,C).

/* declare a new database */

declare_database(Name) :- Name \== compat,
	                  (declared_database(Name)
			  -> true
			  ; (assert(declared_database(Name)),
		             assert(next_free_clause_nr(Name,1)),
			     assert(next_free_mode_nr(Name,1)))).

/* undeclare a database */

undeclare_database(Name) :- Name \== compat,
	                    (declared_database(Name)
			    -> (clear_database(Name),
				retract(declared_database(Name)),
		                retract(next_free_clause_nr(Name,1)),
			        retract(next_free_mode_nr(Name,1)))
			    ; true).

/* new versions extended by database parameter
   fails if database undeclared
*/

print_claus_database_status(Name) :-
	declared_database(Name),
        print('database name: '),print(Name),nl,
	next_free_clause_nr(Name,Nr),
	Total is Nr - 1,
	print('clauses stored in database: '),print(Total),nl,
	next_free_mode_nr(Name,MNr),
	((MNr > 1)
	-> (MT is MNr - 1,
	    print('mode declarations: '),print(MT),nl)
	; (true)
	),
	(claus(Name,_,_,[])
        -> true
	; print('database contains *no* facts')),nl.


/* -------------- */
/* clear_database */
/* -------------- */

/* reset the clause database to empty */

cd(Name) :- clear_database(Name).

/* succeeds even if database undeclared */

clear_database(Name) :- 
	Name \== compat,retract(claus(Name,_Nr,_Head,_Body)),fail.
clear_database(Name) :- 
	Name \== compat,retract(claus_layout(Name,_Nr,_Layout)),fail.
clear_database(Name) :-
	Name \== compat,retract(next_free_clause_nr(Name,_X)),fail.
clear_database(Name) :-
	Name \== compat,retract(mode_declaration(Name,_Nr,_Call,_In,_Any,_Out)),fail.
clear_database(Name) :-
	Name \== compat,retract(next_free_mode_nr(Name,_X)),fail.
clear_database(Name) :-
	Name \== compat,
	assert(next_free_clause_nr(Name,1)),
	assert(next_free_mode_nr(Name,1)).


/* ------------- */
/* read_database */
/* ------------- */

/* reads clauses from standard input and asserts them as claus/4 facts */
/* be sure to issue ?-see('FILE') before using the call and ?-seen after */
/* succeeds even if database undeclared */

/* fails if database undeclared */

%:- use_module(library(dcg_expansion)).

read_database(Name) :-
	declared_database(Name),
	next_free_clause_nr(Name,Nr),
	read_database(Name,Nr,NewNr),
	(Name = compat
	-> retract(next_free_clause_nr(Nr)),assert(next_free_clause_nr(NewNr))
	 ; retract(next_free_clause_nr(Name,Nr)),assert(next_free_clause_nr(Name,NewNr))
	),
	Delta is NewNr - Nr,
	Total is NewNr - 1,
	print('clauses read:   '),print(Delta),
	print('; clauses stored: '),print(Total),nl,
	next_free_mode_nr(Name,MNr),
	((MNr > 1)
	-> (MT is MNr - 1,
	    print('mode declarations stored: '),print(MT),nl)
	; (true)
	).

read_database(Name,Nr,Res) :-
	((
	  %read_term(RTerm,[lines(S,E)]),Layout=[S,E],  <--- CIAO
	  %read_term(RTerm,[layout(Layout)]),  % <---- SICSTUS
	  read_term_with_lines( RTerm , S , E ),
	  %print(read_term_with_lines(RTerm,S,E)),nl,
	  Layout = [S,E],
%      read_term( RTerm , [] ),
	  \+ (RTerm = end_of_file),transform_dcg_term(RTerm,DTerm),
	  transform_clause(DTerm,Term)
	%,  print(transf(Term)),nl
	 )
	-> (Term =.. [Functor|Args],
	    ((Functor=(':-'))
	    -> (/* we have a clause */
		((Args = [Head,BodyCommaList])
		 -> (comma_to_list(BodyCommaList,BodyList),
		     add_clause_with_layout(Name,Nr,Head,BodyList,Layout),Nrp1 is Nr + 1)
		 ;  (treat_query(Name,Args), Nrp1 = Nr)
		)
	       )
	    ;  ((Functor=('-->'))
		-> (/* we have a DCG rule */
			((dcg_translation(Term, ExpandedTerm), %print(exdcg(ExpandedTerm)),nl,
			  ExpandedTerm = ':-'(Head,BodyCommaList)),
			  comma_to_list(BodyCommaList,BodyList),
		      add_clause_with_layout(Name,Nr,Head,BodyList,Layout),Nrp1 is Nr + 1
		     ) -> true
		        ; (print('### Ignored DCG rule: '), print(Term),nl)
		   )
		;  (/* we have a fact */
		    (special_fact(Term) /* e.g. unfold annotations */
		     -> (treat_special_fact(Term), Nrp1 = Nr)
		     ;  (add_clause_with_layout(Name,Nr,Term,[],Layout),Nrp1 is Nr + 1)
		    )
		   )
	       )
	    ),
	    read_database(Name,Nrp1,Res)
	   )
	;  (Res = Nr)
	).

/* add a clause to the database */
add_new_clause(Name,Head,Body) :-
	(Name = compat
	-> (retract(next_free_clause_nr(Nr)),
	    NewNr is Nr + 1,
	    assert(next_free_clause_nr(NewNr)),
	    assert(claus(Nr,Head,Body)))
        ;  (retract(next_free_clause_nr(Name,Nr)),
	    NewNr is Nr + 1,
	    assert(next_free_clause_nr(Name,NewNr)),
	    assert(claus(Name,Nr,Head,Body)))
	).

treat_query(Name,[mode(ModedCall)]) :-
	!,add_new_mode(Name,ModedCall).
treat_query(_Name,[op(Priority,Infix,Op)]) :-
	!,op(Priority,Infix,Op).
treat_query(_Name,Query) :-
	print('### encountered query'),nl,
	print('### '), print(Query),nl.
	/* add_new_clause(q__u__e__r__y,Query). */

/* add a mode declaration to the database */

add_new_mode(compat,ModedCall) :- !,
	retract(next_free_mode_nr(Nr)),
	NewNr is Nr + 1,
	assert(next_free_mode_nr(NewNr)),
	ModedCall =.. [P|MArgs],
	make_mode_declaration(MArgs,VArgs,InArgs,AnyArgs,OutArgs),!,
	VCall =.. [P|VArgs],
	debug_print(mode(VCall,InArgs,AnyArgs,OutArgs)),debug_nl,
	assert(mode_declaration(Nr,VCall,InArgs,AnyArgs,OutArgs)).

add_new_mode(Name,ModedCall) :-
	retract(next_free_mode_nr(Name,Nr)),
	NewNr is Nr + 1,
	assert(next_free_mode_nr(Name,NewNr)),
	ModedCall =.. [P|MArgs],
	make_mode_declaration(MArgs,VArgs,InArgs,AnyArgs,OutArgs),!,
	VCall =.. [P|VArgs],
	debug_print(mode(VCall,InArgs,AnyArgs,OutArgs)),debug_nl,
	assert(mode_declaration(Name,Nr,VCall,InArgs,AnyArgs,OutArgs)).

add_clause(Name,Nr,Head,BodyList) :-
	Head =.. [Predicate|Args],
	length(Args,Arity),!,
	(dont_assert(Predicate,Arity)
	 -> (true)
	 ;  (strip_body(BodyList,StrippedBodyList),
	     (Name = compat
	     -> assert(claus(Nr,Head,StrippedBodyList))
	     ; assert(claus(Name,Nr,Head,StrippedBodyList)))
	    )
	).

/* other predicates */

:- dynamic claus_layout/3.

claus_layout(Nr,Layout) :- claus_layout(compat,Nr,Layout).

add_clause_with_layout(Name,Nr,Head,BodyList,Layout) :-
   add_clause(Name,Nr,Head,BodyList),
    assert(claus_layout(Name,Nr,Layout)).


/* transform elements seperated by commas into a real prolog list */
comma_to_list(Com,Lst) :-
	((nonvar(Com),Com =.. [ ',' , Arg1 , Arg2 ])
	 ->	(comma_to_list(Arg1,L1),
		 comma_to_list(Arg2,L2),
		 append(L1,L2,Lst)
		)
	 ;	(var(Com) -> (Lst = [call(Com)]) ; (Lst = [Com]))
	).

make_mode_declaration(X,[],[],[],[]) :- var(X),!,
	print('### error in mode declaration'),nl.
make_mode_declaration([],[],[],[],[]) :- !.
make_mode_declaration(['i'|T],[V|VT],[V|IT],AT,OT) :- !,
	make_mode_declaration(T,VT,IT,AT,OT).
make_mode_declaration(['-'|T],[V|VT],[V|IT],AT,OT) :- !,
	make_mode_declaration(T,VT,IT,AT,OT).
make_mode_declaration(['o'|T],[V|VT],IT,AT,[V|OT]) :- !,
	make_mode_declaration(T,VT,IT,AT,OT).
make_mode_declaration(['+'|T],[V|VT],IT,AT,[V|OT]) :- !,
	make_mode_declaration(T,VT,IT,AT,OT).
make_mode_declaration(['?'|T],[V|VT],IT,[V|AT],OT) :- !,
	make_mode_declaration(T,VT,IT,AT,OT).
make_mode_declaration([_X|T],[V|VT],IT,[V|AT],OT) :-
	print('### error in mode declaration'),nl,
	make_mode_declaration(T,VT,IT,AT,OT).



/* ================== */
/* transform_clause/2 */
/* ================== */

:- dynamic make_iff_when_reading_clauses/1.
make_iff_when_reading_clauses(off).

transform_clause(Clause,TClause) :-
	make_iff_when_reading_clauses(on),!,
	makeflat(Clause,FClause),
	makeiff(FClause,TClause).
transform_clause(Clause,Clause).

/* ------------------------------------ */
/* set_make_iff_when_reading_clauses/0 */
/* ------------------------------------ */

set_make_iff_when_reading_clauses :-
	print('Transform clauses into prop-iff form when reading in:'),nl,
	print('on: '),nl,
	print('off:'),nl,
	print('Current choice: '),
	make_iff_when_reading_clauses(Cur),
	print(Cur),nl,
	print('choice =>'),
	read(NewValue),
	((\+ (NewValue=on),\+(NewValue=off))
	 ->  print('Illegal value, assuming off'),nl,
	     set_make_iff_when_reading_clauses(off)
	 ;   set_make_iff_when_reading_clauses(NewValue)
	).

/* ----------------------------------- */
/* set_make_iff_when_reading_clauses/1 */
/* ----------------------------------- */

set_make_iff_when_reading_clauses(_NewVal) :-
	retract(make_iff_when_reading_clauses(_Cur)),
	fail.
set_make_iff_when_reading_clauses(NewVal) :-
	asserta(make_iff_when_reading_clauses(NewVal)).

/* ================== */

/* ============================= */
:- dynamic using_ml_typechecker/1.
	/* Using Michael Leuschel Type Checker */
	/* Should be set to true if the object program uses the type checker */
	/* developed by Michael Leuschel */

using_ml_typechecker(yes).

dont_assert(pre_condition,1) :- using_ml_typechecker(yes).
dont_assert(post_condition,1) :- using_ml_typechecker(yes).
dont_assert(type,2) :- using_ml_typechecker(yes).

strip_body([],[]).
strip_body([H|T],Res) :-
	strip_literal(H,SH),
	strip_body(T,ST),
	append(SH,ST,Res).

strip_literal(X,[X]) :- var(X),!.
strip_literal('C'(X,Y,Z),['='(X,[Y|Z])]). /* for DCGs */
strip_literal('\\+'(X),[not(SX)]) :-
	strip_literal(X,[SX]).
strip_literal(not(X),[not(SX)]) :-
	strip_literal(X,[SX]).
strip_literal(when(_Condition,X),[SX]) :-
	strip_literal(X,[SX]).
strip_literal(prepost_call(X),[X]) :- using_ml_typechecker(yes),!.
strip_literal(prepost_mnf_call(X),[X]) :- using_ml_typechecker(yes),!.
strip_literal(mnf_call(X),[X]) :- using_ml_typechecker(yes),!.
strip_literal(verify_pre(_X),[]) :- using_ml_typechecker(yes),!.
strip_literal(verify_post(_X),[]) :- using_ml_typechecker(yes),!.
strip_literal(X,[X]).

/* ============================= */
:- dynamic using_special_facts/1.

using_special_facts(yes).

/* special facts: unfold annotations */
special_fact(X) :- var(X),!,fail.

treat_special_fact(Fact) :-
	var(Fact),!,fail.
treat_special_fact(Fact) :-
	assert(Fact).


/* ===================== */

/* for compatibility */

:- dynamic claus/3.
:- dynamic next_free_clause_nr/1.

:-dynamic mode_declaration/5.
	/* mode(Nr,p(In,Any,Out),[In],[Any],[Out]). */
:- dynamic next_free_mode_nr/1.

next_free_clause_nr(1).
next_free_mode_nr(1).

print_claus_database_status :- print_claus_database_status(compat).

cd :- clear_database.

clear_database :- 
	retract(claus(_Nr,_Head,_Body)),fail.
clear_database :-
	retract(next_free_clause_nr(_X)),fail.
clear_database :-
	retract(mode_declaration(_Nr,_Call,_In,_Any,_Out)),fail.
clear_database :-
	retract(next_free_mode_nr(_X)),fail.
clear_database :-
	assert(next_free_clause_nr(1)),
	assert(next_free_mode_nr(1)).

rd :- read_database.

read_database :- read_database(compat).

read_database(Nr,Res) :- read_database(compat,Nr,Res).

add_new_clause(Head,Body) :- add_new_clause(compat,Head,Body).

treat_query(A) :- treat_query(compat,A).

add_new_mode(ModedCall) :- add_new_mode(compat,ModedCall).

add_clause(Nr,Head,BodyList) :- add_clause(compat,Nr,Head,BodyList).












