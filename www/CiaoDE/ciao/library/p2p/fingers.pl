:- module(fingers,
	[
	    create_fingers_table/3,
	    insert_finger/3,
	    delete_finger/3,
	    finger/3,
	    modify_finger/4,
	    fetch_fingers/2,
	    fetch_max_size/2,
	    fetch_current_size/2,
	    is_empty/1
	],
	[]).

:- use_module(library(lists)).

create_fingers_table(NodeId, MaxSize, FingerTable) :-
	FingerTable = t(NodeId, MaxSize, 0, []).

insert_finger(FingerTable, Finger, NewFingerTable) :-
	FingerTable = t(NodeId, MaxSize, CurrentSize, Fingers),
	NewCurrentSize is CurrentSize + 1,
	NewCurrentSize =< MaxSize,
	NewFingerTable = t(NodeId, MaxSize, NewCurrentSize, [Finger|Fingers]).

delete_finger(FingerTable, Finger, NewFingerTable) :-
	FingerTable = t(NodeId, MaxSize, CurrentSize, Fingers),
	CurrentSize > 0,
	NewCurrentSize is CurrentSize - 1,
	delete_non_ground(Fingers, Finger, NewFingers),
	NewFingerTable = t(NodeId, MaxSize, NewCurrentSize, NewFingers).

finger(Index, FingerTable, Finger) :-
	FingerTable = t(_,_,_,Fingers),
	nth(Index, Fingers, Finger).

modify_finger(Index, FT, Finger, NewFT) :-
	FT = t(N, M, CS, Fingers),
	NewFT = t(N, M, CS, NewFingers),
	length(Fingers, L),
	Index > 0,
	Index =< L,
	substitute(Fingers, 1, Index, Finger, NewFingers).

substitute([_|Fs], Index, Index, NewFinger, [NewFinger|Fs]).
substitute([Finger|Fs], CIndex, Index, NewFinger, NewFingers) :-
	CIndex < Index,
	NewCIndex is CIndex + 1,
	substitute(Fs, NewCIndex, Index, NewFinger, NFs),
	NewFingers = [Finger|NFs].

fetch_fingers(FingerTable, Fingers) :-
	FingerTable = t(_,_,_,Fingers).

fetch_max_size(FingerTable, MaxSize) :-
	FingerTable = t(_,MaxSize,_,_).

fetch_current_size(FingerTable, CurrentSize) :-
	FingerTable = t(_,_,CurrentSize,_).

is_empty(FingerTable) :-
	FingerTable = t(_,_,_,[]).
