
rt(X) :-
	resolve([clause(p,[q]),clause(p,[r]),clause(q,[])],[p],X).

irt1(X) :- 
	resolve([clause(p,[q]),clause(p,[r]),clause(q,[])],X,[]).
irt2(X) :- 
	resolve([clause(p,[q]),clause(p,[r]),clause(q,[])],X,[r]).
irt3(X) :- 
	resolve([clause(p,[q]),clause(p,[r]),clause(q,[])],X,[q,r]).


abdsimple(X) :-
	demo([clause(p,[q]),clause(p,[r]),clause(X,[])],[p]).
abds(X) :-
	demo([clause(p,[q]),clause(p,[r]),clause(q,[]),X],[p]).
abduction(X) :-
	demo([clause(p,[q]),clause(p,[r]),clause(q,[])|X],[p]).
	

synthesise(P) :-
	demo(P,[p,q]).

/* Ecce result: */
irt3__1([p,r]).
irt3__1([q,q,r]).
irt3__1([q,p]).
irt3__1([q,r,q]).

demo(Progr,[]).
demo(Progr,Goal) :-
	resolve(Progr,Goal,NewGoal),
	demo(Progr,NewGoal).

resolve(Progr,[X|T],NewGoal) :-
	member(X,Goal),
	member(clause(X,Bdy),Progr),
	append(Bdy,T,NewGoal).
resolve(Progr,[X|T],[X|NT]) :-
	resolve(Progr,T,NT).

member(X,[X|_T]).
member(X,[Y|T]) :- member(X,T).

append([],L,L).
append([H|X],Y,[H|Z]) :- append(X,Y,Z).
