:- module(dicesum5,_,[fuzzy]).
:- use_module(library(conc_aggregates),[findall/3]).
:- use_module(library(metaterms),[variant/2]).

% this example tries to measure which is the possibility
% that a couple of values, obtained throwing two loaded dice, sum 5. Let
% us suppose we only know that one die is loaded to obtain a small value
% and the other is loaded to obtain a large value. 
%
% the query is  ? sum(5,M)
%


small :# fuzzy_predicate([(1,1),(2,1),(3,0.7),(4,0.3),(5,0),(6,0)]).
large :# fuzzy_predicate([(1,0),(2,0),(3,0.3),(4,0.7),(5,1),(6,1)]).

die1(X,M) :~
	small(X,M).

die2(X,M) :~
	large(X,M).


two_dice(X,Y,M):~ prod
	die1(X,M1),
	die2(Y,M2).

sum(2,M) :~  
	two_dice(1,1,M1).


sum(5,M) :~ dprod
	two_dice(4,1,M1),
	two_dice(1,4,M2),
	two_dice(3,2,M3),
	two_dice(2,3,M4).



sum2(S,M) :~
	{ findall(S^M,p(S,X,Y,M),LM),
	  collect_variants(LM,[],VTs),
	  solutions(VTs,S,Ts),
	  dprodlist(Ts,M) }.

collect_variants([],VTs,VTs).
collect_variants([X^T|XTs],VT0s,VTs):-
    update_variants(VT0s,X,T,VT1s),
    collect_variants(XTs,VT1s,VTs).

update_variants([],X,T,[[X^T]]).
update_variants([VT|VT0s],X,T,VT1s):-
    VT=[V^_|_],
    copy_term(V,V1),
    copy_term(X,X1),
    V1.=. X1,
    true,!,
    VT1s=[[X^T|VT]|VT0s].
update_variants([VT|VT0s],X,T,[VT|VT1s]):-
    update_variants(VT0s,X,T,VT1s).

unifyall([],_,[]).
unifyall([X^T|OT],X,[T|Ts]):-
    unifyall(OT,X,Ts).


solutions(OTs,X,Ts):- member(OT,OTs), unifyall(OT,X,Ts).

p(S,X,Y,M):-
	posint(X,1,6),
	posint(Y,1,6),
	S .=. X + Y,
	two_dice(X,Y,M).


posint(N,N,_).
posint(N,I,D) :- I1 is I+1, I1=<D, posint(N,I1,D).
