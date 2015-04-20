

app([],L,L).
app([H|X],Y,[H|Z]) :- app(X,Y,Z).

rev([],A,A).
rev([H|T],A,R) :- rev(T,[H|A],R).

last([X],X).
last([_|T],X) :- last(T,X).

test1(L,X) :- app(L,[a],L2),last(L2,X).


cpy([],A,A).
cpy([_|T],A,R) :- cpy(T,[a|A],R).

test2(L,X) :- cpy(L,[],L2), last(L2,X).

generate([],_,[]).
generate([H|T],Acc,[NH|NT]) :-
  cpy(Acc,[],NH),
  generate(T,[H|Acc],NT).

test3(L,X) :- generate(L,[],Gen),last(Gen,LL), last(LL,X).

app_rev([],L,[L],R,R).
app_rev([H|X],Y,[H|Z],Acc,Rev) :- app_rev(X,Y,Z,[Y|Acc],Rev).

test4(L,X,Y) :- app_rev(L,a,L2,[],RL),last(L2,X), last(RL,Y).



test5(L,X) :-
   app(L,[a],La),
   rev(La,[],[X|_]).
   

y(s(s(s(X)))) :- z(X).
y(X) :- y(s(s(X))).
z(s(s(X))) :- z(X).
z(0).

path(p(X),p(s(X))).
path(p(s(s(0))),goal).

visit(goal,Acc,Acc).
visit(X,Acc,Res) :-
   \+(member(X,Acc)),
   path(X,Y),
   visit(Y,[X|Acc],Res).
   
   
member(X,[X|_]).
member(X,[_|T]) :- member(X,T).

test6(Res) :- visit(p(0),[],X),rev(X,[],Res).


   

