:- module(dcg_expansion,[phrase/2, phrase/3, dcg_translation/2], 
	[assertions,'compiler/callback',isomodes]).

:- use_module(engine(internals), [term_to_meta/2]).
:- use_module(engine(hiord_rt), [call/1]).
:- use_module(library(terms), [copy_args/3]).

%% FOR TEMPORARILY PARTIALLY DOCUMENTING:
:- use_module(library('assertions/doc_props')).

:- comment(title,"Definite clause grammars (expansion)").

:- comment(author, "Daniel Cabeza").

:- comment(module, "This module implements the Definite clause
   grammars (expansion).").

% Should be something special
:- meta_predicate phrase(goal,?).
:- meta_predicate phrase(goal,?,?).

:- op(1200, xfx,[(-->)]).

:- set_prolog_flag(multi_arity_warnings, off).

:- comment(phrase(Phrase,List),"Like @tt{phrase(Phrase,List,[])}.").

phrase(P, _) :-
	var(P), !,
	throw(error(instantiation_error, phrase/2-1)).
phrase(P, L) :-
        term_to_meta(Pt, P),
	dcg_translate_dcg(Pt, Pt1, L, []), !,
        term_to_meta(Pt1,P1),
        call(P1).

:- pred phrase(+Phrase,?List,?Remainder)
+ doc_incomplete
# "The list @var{List} is a phrase of type @var{Phrase} (according to the
   current grammar rules), where @var{Phrase} is either a non-terminal or more
   generally a grammar rule body. @var{Remainder} is what remains of the list
   after a phrase has been found.".

phrase(P, _, _) :-
	var(P), !,
	throw(error(instantiation_error, phrase/3-1)).
phrase(P, L, R) :- 
        term_to_meta(Pt, P),
	dcg_translate_dcg(Pt, Pt1, L, R), !,
        term_to_meta(Pt1,P1),
        call(P1).

:- comment(dcg_translation/2,"Performs the code expansion of source
	clauses that use DCGs.").

dcg_translation(T1, T3) :-
        dcg_expansion_internal(T1, T2),
        T2 = T3.

dcg_expansion_internal((H,List-->B), (H1:-B2,B1)) :- !,
	dcg_translate_dcg_atom(H, H1, S0, S1),
	dcg_translate_dcg(List, B2, S1, S),
	dcg_translate_dcg(B, B1, S0, S).
dcg_expansion_internal((H-->B), (H1:-B1)) :-
	dcg_translate_dcg_atom(H, H1, S0, S),
	dcg_translate_dcg(B, B1, S0, S).

dcg_translate_dcg(X, Y, S0, S) :-
	dcg_translate_dcg(X, Y0, Tail, S0, S),
	(   Tail\==S0 -> S=Tail, dcg_connect(X, Y0, Y)
	;   dcg_connect(X, Y0, S=Tail, Y)
	).


dcg_connect((_->_), X, X) :- X = (_->_), !.
dcg_connect(_, (P->Q), ((P->Q),true)) :- !.
dcg_connect(_, X, X).

dcg_connect((_->_), (P->Q0), Eq, (P->Q)) :- !, dcg_and(Q0, Eq, Q).
dcg_connect(_, (P->Q), Eq, ((P->Q),Eq)) :- !.
dcg_connect(_, X0, Eq, X) :- dcg_and(X0, Eq, X).

dcg_translate_dcg(X, Y, S, S0, S) :- var(X), !,
        this_module(M),
	Y=M:phrase(X,S0,S),
        add_module_check(ensure_imported(M, phrase, 3)).
dcg_translate_dcg([], true, S0, S0, _) :- !.
dcg_translate_dcg([X|Y], XandY, Tail, S0, S) :- !,
	dcg_translate_dcg(Y, Y1, Tail, S1, S),
	dcg_and('C'(S0,X,S1), Y1, XandY).
dcg_translate_dcg(\+X, (X1 -> fail; S=S0), S, S0, S) :- !,
	dcg_translate_dcg(X, X1, S1, S0, S1).
dcg_translate_dcg((X,Y), XandY, Tail, S0, S) :- !,
	dcg_translate_dcg(X, X1, S1, S0, S1),
	dcg_translate_dcg(Y, Y1, Tail, S1, S),
	dcg_and(X1, Y1, XandY).
dcg_translate_dcg((X->Y), (X1->Y1), Tail, S0, S) :- !,
	dcg_translate_dcg(X, X1, S1, S0, S1),
	dcg_translate_dcg(Y, Y1, Tail, S1, S).
dcg_translate_dcg(if(X,Y,Z), if(X1,Y1,Z1), S, S0, S) :- !,
	dcg_translate_dcg(X, X1, S0, S1),
	dcg_translate_dcg(Y, Y1, S1, S),
	dcg_translate_dcg(Z, Z1, S1, S).
dcg_translate_dcg((X;Y), (X1;Y1), S, S0, S) :- !,
	dcg_translate_dcg(X, X1, S0, S),
	dcg_translate_dcg(Y, Y1, S0, S).
dcg_translate_dcg('|'(X,Y), (X1;Y1), S, S0, S) :- !,
	dcg_translate_dcg(X, X1, S0, S),
	dcg_translate_dcg(Y, Y1, S0, S).
dcg_translate_dcg(!, !, S0, S0, _) :- !.
dcg_translate_dcg({G}, call(G), S0, S0, _) :- var(G), !.
dcg_translate_dcg({G}, G, S0, S0, _) :- !.
dcg_translate_dcg(X, X1, S, S0, S) :-
	dcg_translate_dcg_atom(X, X1, S0, S).

dcg_and(X, Y, Z) :- X==true, !, Z=Y.
dcg_and(X, Y, Z) :- Y==true, !, Z=X.
dcg_and(X, Y, (X,Y)).

dcg_translate_dcg_atom(X, X1, S0, S) :-
	functor(X, F, A),
	A1 is A+1,
	A2 is A+2,
	functor(X1, F, A2),
	arg(A1, X1, S0),
	arg(A2, X1, S),
	copy_args(A, X, X1).
