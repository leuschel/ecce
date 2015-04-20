
:- module(utilities,[length/2]).
:- use_module(fi).
:- use_module(builtin,[is/2]).

length(List,Length):- length1(List,0,Length).

length1([],L,L).
length1([H|List],Acc,Length):- Acc1 is Acc+1, length1(List,Acc1,Length).

%is(X,Y):- builtin:is(X,Y).
