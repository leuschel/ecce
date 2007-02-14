:- module(input,[load_file/1, my_clause/2]).

:- dynamic my_clause/2.

load_file(F) :-
    retractall(my_clause(_,_)),
	see(F),
	remember_all,
	seen.

remember_all :-
	read(C),
	(
	    C == end_of_file -> true
	;
	    remember_clause(C),
	    remember_all
	).

remember_clause((A :- Body)) :-
	!,
        normalize(A,ANorm,As1-As2),
	tuple2list(Body,As2-[]),
	assert(my_clause(ANorm,As1)).

remember_clause(A) :-
        normalize(A,ANorm,Unifs-[]),
	assert(my_clause(ANorm,Unifs)).


normalize(Atom,ANorm,DUs) :-
    Atom =.. [Pred|Args],
    normalize_args(Args,VArgs,DUs),
    (Pred == (=) -> NewPred = unify ; NewPred=Pred),	
    ANorm =.. [NewPred|VArgs].

normalize_args([],[],X-X).
normalize_args([T|Ts], [V|Vs], [unify(V,T)|Us]-Us1) :-
    normalize_args(Ts,Vs,Us-Us1).


tuple2list((B,Bs),Bs1-Bs3) :-
	!,
        normalize(B,BNorm,Bs1-[BNorm|Bs2]),
	tuple2list(Bs,Bs2-Bs3).

tuple2list(B,Bs1-Bs2) :-
    normalize(B,BNorm,Bs1-[BNorm|Bs2]).




