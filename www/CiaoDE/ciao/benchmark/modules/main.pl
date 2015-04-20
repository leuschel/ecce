
:- use_module(foo,[p/1]).
%:- use_module(utilities,[length/2]).

main:-
%X=3,
%X is 3,
%length([1,2,3],X),
%q(Y), length(Y,X),
p(X),
display(X).%, write(X).

/*
p(N) :- q(X), length(X,N).

p1(N):- q(X), length1(X,0,N).

q([1,2,3,4]).

length(List,Length):- length1(List,0,Length).

length1([],L,L).
length1([H|List],Acc,Length):- Acc1 is Acc+1, length1(List,Acc1,Length).

%is(X,Y):- builtin:is(X,Y).

*/
