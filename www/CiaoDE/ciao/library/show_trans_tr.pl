:- module(show_trans_tr, [show/3], []).

show(clause(0,0),_,M) :- !,
        message(['Module ',M,':']).
show(clause(H,true),_,_) :- !,
        message([''(H),'.']), fail.
show(clause(H,B),_,_) :-
        message([''(H), ' :-\n','  ',''(B),'.']), fail.
