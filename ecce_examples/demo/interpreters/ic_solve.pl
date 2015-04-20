
test(X) :- inc_solve([iv],[male(X)]).
test(X,Y) :- inc_solve([iv],[edge(X,Y)]).


inc_solve([H|T],AddedFacts) :-
   mem(H,AddedFacts),
   solve(T,AddedFacts).
inc_solve([H|T],AddedFacts) :-
   rule(H,Bdy),
   app(Bdy,T,NGoal),
   inc_solve(NGoal,AddedFacts).
   

solve([],_).
solve([H|T],Add) :-
  nrule(H,Bdy,Add),
  solve(Bdy,Add),
  solve(T,Add).
  
  
mem(X,[X|_]).
mem(X,[_|T]) :- mem(X,T).

app([],X,X).
app([H|X],Y,[H|Z]) :- app(X,Y,Z).

nrule(H,_,Add) :- mem(H,Add).
nrule(H,B,_) :- rule(H,B).

rule(iv,[path(X,X)]).
rule(iv,[male(X),female(X)]).
rule(path(X,Y),[edge(X,Y)]).
rule(path(X,Z),[edge(X,Y),path(Y,Z)]).
rule(edge(X,Y),[]) :- edge(X,Y).
rule(male(X),[]) :- male(X).
rule(female(X),[]) :- female(X).


iv :- path(X,X). /* integrity violated */

path(X,Y) :- edge(X,Y).
path(X,Z) :- edge(X,Y), path(Y,Z).

