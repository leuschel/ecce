mqueens(1,[1]):- !.
mqueens(N,L):-
    N1 is N-1,
    mqueens(N1,L1),
    expand_col(L1,N1,L).

mqueens(N,L):-
    N1 is N-1,
    mqueens(N1,L1),
    expand_row(L1,N,L).

expand_col(L,N,[none|L]):-
    all_unsave(L,N),!.
expand_col(L,N,L1):-
    expand_col1(L,N,L1).

expand_col1(L,N,[N|L]):-
    save(L,N).
expand_col1(L,N,L1):-
    N1 is N-1,
    expand_col1(L,N1,L1).

all_unsave(L,0).
all_unsave(L,N):- save(L,N),!,fail.
all_unsave(L,N):- N1 is N-1,
    all_unsave(L,N1).

save(L,N):- save1(L,1,N).

save1([],_,_).
save1([P|L],C,P):- !,fail.
save1([P|L],C,C):- !,fail.
save1([P|L],C,N):- A is abs(1-C),A is abs(N-P),!,fail.
save1(L,C,N).

expand_row(L1,N1,L):-
    N is N1+1,
    modify_col([],L1,N,L2),
    expand_col(L2,N1,L).

modify_col(L2,[],N,_):- fail.
modify_col(L2,[C|L1],N,L):- append(L2,[N|L1],L),
    all_save(L).

modify_col(L2,[C|L1],N,L):-
    append(L2,[C],L3),
    modify_col(L3,N,L).

all_save([_]).
all_save([C|L]):-
    save(L),
    save(L,C).


append([],L,L).
append([X|Xs],L,[X|Zs]):- append(Xs,L,Zs).
