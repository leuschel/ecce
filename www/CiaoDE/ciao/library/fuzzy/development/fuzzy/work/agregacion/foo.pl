:- module(foo,_,[fuzzy]).

:- use_package(clpr).
:- use_module(library(aggregates)).
:- use_module(library(metaterms)).
:- use_module(library(sort)).
:- use_package(library(hiord)).

p(f(_),0.5).
p(f(a),0.8).

predall(X,M):-
	all(_,p(X,_),M).

sol(f(_),0.8).
%sol(f(a),0.7).
sol(g(_),0.6).


several(f(X),X).
several(g(X),X).

some(X,T):-
    sol(X,T).
some(X,T):-
    sol(X,Tx),
    sol(Y,Ty),
    several(X,Y),
    T.=.1/3*(2*Tx+Ty).

best(X,T):-
    all(max(X),some(X,_),T).

all(max(X),some(X,T0),T):-
% %    findall(T0,p(X,T0),Ts),display(Ts),nl,
% % %    findall(X^T0,some(X,T0),STs),
% % %    iden(STs,X,Ts),
      findall(X^T0,some(X,T0),STs), display(STs),nl,
      collect_variants(STs,[],VTs), display(VTs),nl,
      order_instances(VTs,OTs), display(OTs),nl,
      solutions(OTs,X,Ts), display(Ts),nl,
     maxim(Ts,T).

collect_variants([],VTs,VTs).
collect_variants([X^T|XTs],VT0s,VTs):-
    update_variants(VT0s,X,T,VT1s),
    collect_variants(XTs,VT1s,VTs).

update_variants([],X,T,[[X^T]]).
update_variants([VT|VT0s],X,T,VT1s):-
    VT=[V^_|_],
    instance(V,X), !,
    VT1s=[[X^T|VT]|VT0s].
update_variants([VT|VT0s],X,T,[VT|VT1s]):-
    update_variants(VT0s,X,T,VT1s).

order_instances(VTs,OTs):- sort(VTs,OTs).

solutions(OTs,X,Ts):- member(OT,OTs), unifyall(OT,X,Ts).

unifyall([],_,[]).
unifyall([X^T|OT],X,[T|Ts]):-
    unifyall(OT,X,Ts).

max(X,Y,Z):- X.>=.Y, Z=X.
max(X,Y,Z):- X.<.Y, Z=Y.

prod(X,Y,Z):- X*Y.=.Z.

maxim([X],X).
maxim([X,Y|Xs],M):-
    max(X,Y,Z),
    maxim([Z|Xs],M).


% middle(X,T):-
%    int(X),
%    X.>=.1,
%    X.=<.40,
%    middle0(X,T).


% middle(X,T):-
%    posint(X,1,40),
%    middle0(X,T).

% posint(N,N,_).
% posint(N,I,D) :- I1 is I+1, I1=<D, posint(N,I1,D).


% middle(X,T):-
%    int(X),
%    middle0(X,T).

% middle0(X,0):- X.=<.21.
% middle0(X,0.8):- X.>=.22, X.=<.32.
% middle0(X,0):- X.>=.33.

% young(X,T):-
%     young0(X,T),
%     int(X).

% young0(X,1):- X .>=. 0, X .<. 35.
% young0(X,M):- X .>=. 35, X .<. 45, 10*M .=. 45-X.
% young0(X,0):- X .>=. 45, X .=<. 120. 


small :# fuzzy_predicate([(1,1),(2,1),(3,0.7),(4,0.3),(5,0),(6,0)]).
large :# fuzzy_predicate([(1,0),(2,0),(3,0.3),(4,0.7),(5,1),(6,1)]).

die1(X,M) :~
	small(X,M).

die2(X,M) :~
	large(X,M).


two_dice(X,Y,M):~ prod
	die1(X,M1),
	die2(Y,M2).


msum(X,M) :-
	X .=. Y + Z,
	all(max,(two_dice(Y,Z,_)),M).

all(max,(two_dice(Y,Z,_)),T):-
%    findall(T0,(two_dice(Y,Z,T0),X .=. Y + Z),Ts),display(Ts),nl,
%    maxim(Ts,T).
     findall((Y,Z)^T0,two_dice(Y,Z,T0),STs),display(STs),nl,
     collect_variants(STs,[],VTs),display(VTs),nl,
     order_instances(VTs,OTs),display(OTs),nl,
     solutions(OTs,X,Ts),display(Ts),nl,
    maxim(Ts,T).


% all(max(X),some(X,T0),T):-
% %    findall(T0,p(X,T0),Ts),display(Ts),nl,
% % %    findall(X^T0,some(X,T0),STs),
% % %    iden(STs,X,Ts),
%      findall(X^T0,some(X,T0),STs),display(STs),nl,
%      collect_variants(STs,[],VTs),display(VTs),nl,
%      order_instances(VTs,OTs),display(OTs),nl,
%      solutions(OTs,X,Ts),display(Ts),nl,
%     maxim(Ts,T).
