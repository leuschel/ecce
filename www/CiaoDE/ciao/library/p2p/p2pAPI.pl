:- module(p2pAPI,
	[
	    '$createNet'/4,
	    '$joinNet'/4,
	    ping/0,
% 	    leaveNet/0,
 	    send/3,
	    receive/2,
% 	    multicast/3,
% 	    broadcast/1,
% 	    sendToSucc/2,
 	    getNetConfig/2,
 	    getNodeConfig/1,
 	    ap2NodeId/2,
% 	    getAPConfig/1,
	    get_AP/3,
	    getLocalAddress/2
%	    ap2address/2
	], 
	[remote]).

:- use_module(library('p2p/chord'),
	[
	    find_successor/4, 
	    set_successor/2, 
	    set_finger/3, 
	    notify/3
	]).
:- use_module(library(sockets), [socket_accept/2]).
:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(concurrency)).
:- use_module(library(sha1)).

:- data netConfig/4.
:- data apConfig/3.

% MaxNetSize: 0..2^ExpFactor - 1
% SuccListSize unneeded
'$createNet'(NetConfig, APConfig, NodeId, Connection) :-
	NetConfig = netConfig(Protocol, MaxNetSize, ExpFactor),
	setAPConfig(APConfig, NodeId, Connection),
	asserta_fact(netConfig(NodeId, Protocol, MaxNetSize, ExpFactor)).

'$joinNet'(RemoteAP, APConfig, NodeId, Connection) :-
% 	RemoteAP = apConfig(IP, Port),
% 	get_AP(IP, Port, AP),
	setAPConfig(APConfig, NodeId, Connection),
	ap2NodeId(RemoteAP, RemoteNId),
	send(RemoteAP, getNetConfig(RemoteNId, netConfig(RemoteNId, P, M, E)), sync),
	asserta_fact(netConfig(NodeId, P, M, E)).
% I'll catch the result from the message collector
	

setAPConfig(APConfig, NodeId, Connection) :-
	APConfig = a(IP, Port),
	set_connection(Port, Connection),
	ap2NodeId(APConfig, NodeId),
	asserta_fact(apConfig(NodeId, IP, Port)).

% This will eventually be complemented with a call to a hash function
ap2NodeId(a(IP, Port), NodeId) :-
	atom_number(APort, Port),
	atom_concat([IP, ':', APort], ANodeId),
	atom_codes(ANodeId, CNodeId),
%	display(CNodeId), nl,
	sha1(CNodeId, Key), %To be fixed!
	number_codes(NodeId, Key).

getNetConfig(NodeId, netConfig(NodeId, Protocol, MaxNetSize, ExpFactor)) :-
	current_fact(netConfig(NodeId, Protocol, MaxNetSize, ExpFactor)).

getNodeConfig(NodeId) :-
	current_fact(netConfig(NodeId,_,_,_)).

getLocalAddress(NodeId, AP) :-
	current_fact(apConfig(NodeId, Ip, Port)),
	get_AP(Ip, Port, AP).

% ap2address(apConfig(IP, Port), AP) :-
% 	get_AP(IP, Port, AP).

get_AP(IP, Port, a(IP, Port)).

ping.

send(AP, Msg, Mode) :-
%	ap2address(AP, Address),
	receive(Mode, Msg) @ AP.

receive(async, Msg) :-
	eng_call(call(Msg), create, create).
receive(sync, Msg) :-
	call(Msg).
