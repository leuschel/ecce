

/* Unification of ground terms with occurs check */
/* program taken from a Lopstr'92 article by */
/*	J. Gallagher & D.A. de Waal: 30/6/91 */

/* test: ?-unify(struct(f,[var(1),var(2)]),struct(f,[var(3),var(3)]),S). */
/* test: ?-unify(struct(f,[var(1),var(1)]),struct(f,[var(2),var(3)]),S). */

/* pe: unify(struct(p,[X]),struct(p,[struct(a,[])]),Sub). */
/* pe: unify(X,struct(f,[Y]),Sub). */

groundunify:unify(X,Y,S) :-
	groundunify:unify(X,Y,[],S).




groundunify:unify(var(N),T,S,S1) :-
	groundunify:bound(var(N),S,B,V),
	groundunify:unify(var(N),T,S,S1,B,V).
groundunify:unify(struct(F,Args),var(N),S,S1) :-
	groundunify:unify(var(N),struct(F,Args),S,S1).
groundunify:unify(struct(F,Args1),struct(F,Args2),S,S2) :-
	groundunify:unifyargs(Args1,Args2,S,S2).


groundunify:unify(var(_),T,S,S1,B,true) :-
	groundunify:unify(B,T,S,S1).
groundunify:unify(var(N),T,S,S1,_,false) :-
	groundunify:unify1(T,var(N),S,S1).


groundunify:unifyargs([],[],S,S).
groundunify:unifyargs([T|Ts],[R|Rs],S,S2) :-
	groundunify:unify(T,R,S,S1),
	groundunify:unifyargs(Ts,Rs,S1,S2).


groundunify:unify1(struct(F,Args),var(N),S,[var(N)/struct(F,Args)|S]) :-
	\+(occur_args(var(N),Args,S)).
groundunify:unify1(var(N),var(N),S,S).
groundunify:unify1(var(M),var(N),S,S1) :-
	M \== N,
	groundunify:bound(var(M),S,B,V),
	groundunify:unify1(var(M),var(N),S,S1,B,V).
groundunify:unify1(var(_),var(N),S,S1,B,true) :-
	groundunify:unify1(B,var(N),S,S1).
groundunify:unify1(var(M),var(N),S,[var(N)/var(M)|S],_,false).



groundunify:bound(var(N),[var(N)/T|_],T,true) :-
	T \== var(N).
groundunify:bound(var(N),[B/_|S],T,F) :-
	B \== var(N),
	groundunify:bound(var(N),S,T,F).
groundunify:bound(var(_),[],_,false).



groundunify:dereference(var(N),[var(N)/T|_],T) :-
	T \== var(N).
groundunify:dereference(var(N),[B/_|S],T) :-
	B \== var(N),
	groundunify:dereference(var(N),S,T).



groundunify:occur(var(N),var(M),S) :-
	groundunify:dereference(var(M),S,T),
	groundunify:occur(var(N),T,S).
groundunify:occur(var(N),var(N),_).
groundunify:occur(var(N),struct(_,Args),S) :-
	groundunify:occur_args(var(N),Args,S).

groundunify:occur_args(var(N),[A|_],S) :-
	groundunify:occur(var(N),A,S).
groundunify:occur_args(var(N),[_|As],S) :-
	groundunify:occur_args(var(N),As,S).


