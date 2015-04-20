:- module(chord, 
	[
	    successor/3,
	    find_successor/4,
	    set_successor/2,
	    predecessor/3,
	    createNet/4,
	    joinNet/4,
	    stabilize/1,
	    notify/3,
	    fix_fingers/1,
	    set_finger/3,
	    check_predecessor/1
	],
	[remote]).
	    
:- use_module(library('p2p/fingers')).
%:- use_module(library(concurrency)).
:- use_module(library('p2p/p2pAPI')).
:- reexport(library('p2p/p2pAPI')).

:- data pred/3.
:- data succ/3.
:- data fingers/2.

successor(NodeId, S, SAP) :-
	var(S),
	current_fact(succ(NodeId, S, SAP)).
successor(NodeId, S, SAP) :-
	retract_fact(succ(NodeId,_,_)),
	asserta_fact(succ(NodeId, S, SAP)).
successor(NodeId, S, SAP) :-
	asserta_fact(succ(NodeId, S, SAP)).

predecessor(NodeId, P, PAP) :-
	var(P),
	current_fact(pred(NodeId, P, PAP)).
predecessor(NodeId, P, PAP) :-
	retract_fact(pred(NodeId,_,_)),
	asserta_fact(pred(NodeId, P, PAP)).
predecessor(NodeId, P, PAP) :-
	asserta_fact(pred(NodeId, P, PAP)).


% I'm asking for Id's successor for a node sited at ReturnAdd
find_successor(Id, LocalNodeId, ReturnAP, ReturnAcc) :-
	display(successor(LocalNodeId, LocalSucc, LocalSAP)), nl,
	successor(LocalNodeId, LocalSucc, LocalSAP),
	do_find_successor(Id, LocalNodeId, LocalSucc,
	                  LocalSAP, ReturnAP, ReturnAcc),
	display(end), nl.

do_find_successor(Id, N, S, SAP, ReturnAP, ReturnAcc) :-
	Id > N,
	Id =< S,
	ap2NodeId(ReturnAP, ReturnNodeId),
	switch_reply(ReturnAcc, ReturnAP, ReturnNodeId, S, SAP).
do_find_successor(Id, N,_,_,ReturnAP, ReturnAcc) :-
	current_fact(fingers(N, FT)),
	(is_empty(FT) ->
	 empty_fingers_f_s(Id, N, ReturnAP)
	;
	 closest_preceding_node(N, Id, NPrime),
	 finger2Id_ap(NPrime, NPrimeId, NPrimeAP),
	 send(NPrimeAP, find_succesor(Id,NPrimeId,ReturnAP,ReturnAcc), async)
	).

switch_reply(set_s, ReturnAP, ReturnNodeId, S, SAP) :-
	send(ReturnAP, set_successor(ReturnNodeId, s(S, SAP)), sync).
switch_reply(set_f(Index), ReturnAP, ReturnNodeId, F, FAP) :-
	send(ReturnAP, set_finger(ReturnNodeId, Index, f(F, FAP)), sync).

set_successor(Id, s(S, SAP)) :-
	successor(Id, S, SAP).

createNet(NetConfig, APConfig, NodeId, Connection) :-
	'$createNet'(NetConfig, APConfig, NodeId, Connection),
	predecessor(NodeId, nil, nil),
	successor(NodeId, NodeId, APConfig),
	createFingers(NodeId).

joinNet(RemoteAP, APConfig, NodeId, Connection) :-
	'$joinNet'(RemoteAP, APConfig, NodeId, Connection),
	predecessor(NodeId, nil, nil),
	createFingers(NodeId),
%	fix_fingers(NodeId),
	ap2NodeId(RemoteAP, RemoteId),
	send(RemoteAP, find_successor(NodeId,RemoteId,APConfig,set_s), async).

createFingers(NodeId) :-
	getNetConfig(NodeId, netConfig(NodeId,_,_,ExpFactor)),
	create_fingers_table(NodeId, ExpFactor, FingerTable),
	asserta_fact(fingers(NodeId, FingerTable)).

empty_fingers_f_s(Id, LocalNodeId, ReturnAP) :-
	LocalNodeId >= Id,
	getLocalAddress(LocalNodeId, AP),
	send(ReturnAP, set_successor(Id, s(LocalNodeId, AP)), sync).
empty_fingers_f_s(Id, LocalNodeId, ReturnAP) :-
	set_successor(LocalNodeId, s(Id, ReturnAP)),
	getLocalAddress(LocalNodeId, AP),
	send(ReturnAP, notify(Id, LocalNodeId, AP), sync).

stabilize(NodeId) :-
	successor(NodeId, S, SAP),
	send(SAP, predecessor(S, SPred, SPredAP), sync),
	do_stabilize(NodeId, S, SPred, SPredAP, NewS, NewSAP),
	getLocalAddress(NodeId, AP),
	send(NewSAP, notify(NewS, NodeId, AP), sync).
do_stabilize(NodeId, S, SPred, SPredAP, SPred, SPredAP) :-
	SPred > NodeId,
	SPred < S,
	successor(NodeId, SPred, SPredAP).
do_stabilize(NodeId,S,_,_,S, SAP) :-
	successor(NodeId,_,SAP).

notify(LocalNodeId, Id, IdAP) :-
	predecessor(LocalNodeId, Pred,_),
	do_notify(Pred, LocalNodeId, Id, IdAP).
do_notify(nil, LocalNodeId, Id, IdAP) :-
	predecessor(LocalNodeId, Id, IdAP).
do_notify(Pred, LocalNodeId, Id, IdAP) :-
	Pred < Id,
	Id < LocalNodeId,
	predecessor(LocalNodeId, Id, IdAP).
	
fix_fingers(NodeId) :-
 	fingers(NodeId, FT),
 	fetch_max_size(FT, M),
	do_fix_fingers(NodeId, 1, M).

do_fix_fingers(_,M, M).
do_fix_fingers(NodeId, Index, M) :-
	Mod is integer(2**M),
	Id is (NodeId + integer(2**(Index - 1))) mod Mod,
	getLocalAddress(NodeId, AP),
	find_successor(Id, NodeId, AP, set_f(Index)), 
	NewIndex is Index + 1,
	do_fix_fingers(NodeId, NewIndex, M).

set_finger(NodeId, Index, Finger) :-
	retract_fact(fingers(NodeId, FT)),
	fetch_current_size(FT, CSize), 
	insert_or_modify(CSize, Index, Finger, FT, NewFT),
	asserta_fact(fingers(NodeId, NewFT)).

insert_or_modify(CSize, Index, Finger, FT, NewFT):-
	CSize >= Index,
	modify_finger(Index, FT, Finger, NewFT).
insert_or_modify(_,_, Finger, FT, NewFT):-
	insert_finger(FT, Finger, NewFT).

check_predecessor(NodeId) :-
	predecessor(NodeId,_,PAP),
	(send(PAP, ping, sync) ->
	 true
	;
	 predecessor(NodeId, nil, nil)
	).

closest_preceding_node(LocalNodeId, Id, Finger) :-
	current_fact(fingers(LocalNodeId, FingerTable)),
	fetch_fingers(FingerTable, Fingers),
	travel_fingers(Fingers, LocalNodeId, Id, Finger),
	finger2Id_ap(Finger,_,FAP),
	send(FAP, ping, sync).
closest_preceding_node(LocalNodeId,_, f(LocalNodeId, LocalAP)) :-
	getLocalAddress(LocalNodeId, LocalAP).	

travel_fingers([], LocalNodeId,_,LocalNodeId).
travel_fingers([Finger|_], LocalNodeId, Id, Finger) :-
	finger2Id_ap(Finger, FId,_),
	LocalNodeId < FId,
	FId < Id.
travel_fingers([_|Fs], LocalNodeId, Id, Finger) :-
	travel_fingers(Fs, LocalNodeId, Id, Finger).

finger2Id_ap(f(Id,AP), Id, AP).
