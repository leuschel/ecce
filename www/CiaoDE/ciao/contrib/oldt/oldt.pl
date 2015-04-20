
:- use_package([assertions,default]).

:- comment(module,"This code allows tabulated execution of a goal.
The code has to be loaded within the module where the goal is defined.
It has to be dynamic, too.
The goal has to be wrapped in a @tt{oldt/1} meta-call with module
qualification, e.g.: @tt{oldt(user:path(X,Y))}.
").

% solved (but introduced #3)
%:- comment(bug,"#1 The definition of that goal has to be ""self-contained"":
%	 i.e., it won't work with imported predicates.").
:- comment(bug,"#2 If tabulated execution is aborted the state is
	inconsistent: you better reload everything!").
:- comment(bug,"#3 Does not work with mutually recursive predicates!").
:- comment(bug,"#4 A check for table completeness is required, so that
	subsequent calls only consume the table. Currently, it makes
        little sense to oldt(G) if G is not recursive.").

:- use_module(engine(internals), [term_to_meta/2]).

:- data tabulated/2, table/1, changed/2. %, goal/0.

/* Should be:
oldt(X):-
	module_expand(X,G),
	solve(G).
This does NOT do the trick: ;-)
oldt(X):-
	assert( (goal:- X) ),
	retract( (goal:- G) ),
	solve(G).
Ugly, but works:
*/
oldt(M:X):-
	X =.. [F|As],
	atom_concat(M,:,Q),
	atom_concat(Q,F,N),
	G =.. [N|As],
	resolve(G).

resolve(G):-
	functor(G,F,A),
	functor(X,F,A),
	( tabulated(F,A) -> true
	; assert(tabulated(F,A)) ),
	( table(G) % there is no guarantee that the table is exhaustive, so:
	; compute(X,F,A),
	  G=X
	).

%:- meta_predicate(solve(goal)).

solve('basiccontrol:true'):- !.
solve('basiccontrol:,'(X,Y)):- !,
	solve(X),
	solve(Y).
solve(X):-
	functor(X,F,A),
	tabulated(F,A), !,
	table(X).
solve(X):-
	term_to_meta(X,G),
	call(G).

compute(G,F,A):-
	term_to_meta(G,X),
	clause(X,Y),
	solve(Y),       %   ( Y==true -> retract(X) ; true ),
	tabulate(G,F,A).
compute(X,F,A):-
	retract(changed(F,A)),
	compute(X,F,A).

tabulate(X,_F,_A):-
	copy_term(X,Y),
	table(Y),
	renamed(X,Y), !,
%1      true.
%2      fail.
	fail.
tabulate(X,F,A):-
	assert(table(X)),
	assert(changed(F,A)).

renamed(Term1,Term2) :-
	\+ \+
        (  numbervars(Term1,0,N),
	   numbervars(Term2,0,N),
	   Term1 = Term2
        ).
