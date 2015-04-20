:- module(agency, [main/0, agent_process_message/1], [actmods]).

:- use_package(remote).
%:- include(library(hlc)).

%:- use_package(actmods).
:- use_module(library('actmods/webbased_locate')).
:- use_active_module(binder, 
	                     [address/2, 
 			        add_address/2, 
			        remove_address/1, 
			        get_all_agencies/1]).

:- use_package(fd).
:- use_module(library(concurrency)).
:- use_module(library(system)).
:- use_module(library(lists)).
:- use_module(library(dynamic)).
:- use_module(library(aggregates)).



% message contains a message and a worker ID
:- concurrent msg/2.
% Stores the nodes of the search tree that currently belong to a worker
:- concurrent r_s/2.
% The same, but for share messages.
:- concurrent sh/2.
% Manager address
:- data manager_address/1.
% Local host
:- data local_address/1.
% Store of messages to send
:- concurrent m/1.
% Total number of workers involved
:- data number_of_workers/1.
% Original store: Worker, Store, Politics
:- data store/4.

:- multifile is_boundto/1.
:- concurrent is_boundto/1.

main :- 
	eng_call(register_to_binder, create, create),
	serve_random_port.

register_to_binder :-
	pause(1),
	is_boundto(Port),
	display('Agency bound to port: '),
	display(Port), nl,
	current_host(Host),
	add_address(agency, a(Host, Port)),
	asserta_fact(local_address(a(Host, Port))),
	eng_call(send_message, create, create).

send_message :-
	retract_fact(m(M)),
	do_m2manager(M),
	send_message.

agent_process_message(Message) :-
	do_agent_process_message(Message).

explore_list(_,[]).
explore_list(W, [d(Done, LDS)|Ds]) :- 
%	display('Agency serving message: '),
	display(explore(W, Done)),nl,
	asserta_fact(r_s(W, d(Done, LDS))),
	asserta_fact(msg(W, explore)),
	explore_list(W, Ds).

do_agent_process_message(e(W, Store, DoneLists, Politics)):- !,
	asserta_fact(store(W, Store, Politics, 20)),
	explore_list(W, DoneLists).
do_agent_process_message(r_e(W, DoneLists)):- !,
	explore_list(W, DoneLists).
do_agent_process_message(share(W)) :- !,
% 	display('Agency serving message: '),
 	display(share(W)),nl,
	asserta_fact(sh(W, ok)),
	asserta_fact(msg(W, share)).
do_agent_process_message(stop(W)) :- !,
% 	display('Agency serving message: '),
 	display(stop(W)),nl,
	asserta_fact(msg(W, stop)).
do_agent_process_message(_).

set_manager(Manager) :-
% 	display('Registering my manager: '),
 	display(manager(Manager)), nl, 
	asserta_fact(manager_address(Manager)).

set_num_workers(NumW) :-
	asserta_fact(number_of_workers(NumW)),
	display(number_of_workers(NumW)),nl.

% Receives from the manager the number of workers this agency has to run

init_workers(N, [N|Ws]) :-
	N > 0, !,
	display('Agency launching worker: '),
	eng_call(worker(N), create, create),
%	display(W),nl,
	asserta_fact(r_s(N, bottom)),
	asserta_fact(sh(N, ko)),
	N1 is N - 1,
	init_workers(N1, Ws).
init_workers(0, []) .

worker(N) :- 
% 	display(agency_worker(N)), nl,
	retract_fact(msg(N, Message)),
% 	display(proc_msg(N, Message)), nl,
	process_message(Message, N),
	worker(N).

process_message(explore, W) :- !,
%  	current_fact(r_s(W, DoneTuple)),
%   	display('Retracting R_S'(DoneTuple)), nl,
	retract_fact(r_s(W, DoneTuple)),
%	display(retracted),nl,
	(DoneTuple = bottom ->
%	 display(bottom),nl,
	 asserta_fact(r_s(W, bottom)),
	 request_work(W)
	;
	 DoneTuple = d(Done, _LDS),
%	 display(DoneTuple),nl,
	 process_store(W, Store, Politics),
%	 display('Entering evaluation of the store'(Store)),nl,
	 (eval_store(Done, Store) ->
%	  display(exploring(Store, r_s(W, d(Done, LDS)))),nl,
	  (ground(Store) ->
	   m2manager(collect(Store, Politics))
	  ;
	   commit_c(Store, r_s(W, DoneTuple)),
	   current_fact(r_s(W,D)),
%	   display(queda(r_s(W,D))),nl,
	   ( D = bottom->
	    asserta_fact(msg(W, explore))
	   ;
	    true),
%	   display('Commit done'),nl,
	   recall(W))
%	   display('Recall done'),nl)
	 ;
	   true)).
process_message(share, W) :- !,
	retract_fact(sh(W, _)),
	display(share(W)),nl,
	eng_call(work_to_share(W), create, create).
%	work_to_share(W).
process_message(stop, W) :- !,
	display(stopping(W)),nl,
	retractall(r_s(W,_)),
	retractall(msg(W,_)).
process_message(_,_).

process_store(W, Store, Politics) :-
%  	retract_fact(store(W, OldStore, Politics, LifeLeft)),
%   	(LifeLeft = 0 ->
%   	 copy_term(OldStore, Store),
%   	 NLifeLeft = 20
%     	;
%    	 Store = OldStore,
%    	 NLifeLeft is LifeLeft - 1), 
% 	asserta_fact(store(W, Store, Politics, NLifeLeft)).
	current_fact(store(W, Store, Politics, _NLifeLeft)).

commit_c(EvStore, r_s(W, d(Done, LDS))) :-
	display(evS(EvStore)),nl,
	choose_my_var(EvStore, V),
%	display(choose_var(V)),nl,
	pitm(V, Point),
%	display(pitm(Point)),nl,
	calculate_LDS(LDS, NewLDS),
	add_constraint(V, EvStore, Point, r_s(W, d(Done, NewLDS))),
%	display('Added new constraint'),nl,
	fail.
commit_c(_,_).

add_constraint(V, _, Point, r_s(W, d(Done, LDS))) :-
%	display('>'(V,Point)),nl,
	V .>. Point,
%	display(done),nl,
	insert_last(Done, '>'(Point), NewDone),
	include_r_s(LDS, W, r_s(W, d(NewDone, LDS))).
add_constraint(V, _, Point, r_s(W, d(Done, LDS))) :-
%	display('<'(V,Point)),nl,
	V .<. Point,
%	display(done),nl,
	insert_last(Done, '<'(Point), NewDone),
	include_r_s(LDS, W, r_s(W, d(NewDone, LDS))).
% For the sake of performance it is important to open the way with "="
add_constraint(V, EvStore, Point, r_s(W, d(Done, LDS))) :-
%	display('='(V,Point)),nl,
	V = Point,
	eng_call(exhaustive_local_search(W, EvStore), create, create).
%	display(done),nl,
% 	insert_last(Done, '='(Point), NewDone),
% 	include_r_s(LDS, W, r_s(W, d(NewDone, LDS))).

exhaustive_local_search(W, EvStore) :-
	do_exhaustive_local_search(W, EvStore).

do_exhaustive_local_search(W, EvStore) :-
	process_store(W,_, Politics)
	labeling(EvStore), 
	m2manager(collect(EvStore, Politics)),
	fail.
do_exhaustive_local_search(_,_).

include_r_s(0, W, R_S) :- 
%	display(about_to_assert_R_S),nl,
	asserta_fact(msg(W, explore)),
	asserta_fact(R_S).
%	display(asserted(R_S)),nl.
include_r_s(LDS, W, R_S) :- 
	LDS > 0, 
	Bottom = r_s(W, bottom),
%	display(lds(LDS)),nl,
	retract_fact(Bottom),
%	display(a),nl,
	assertz_fact(R_S),
%	display(b),nl,
	assertz_fact(Bottom),
%	display(c),nl,
	asserta_fact(msg(W, explore)), !.
%	display('Asserted_new_explore_msg'),nl.

choose_my_var(Vs, V) :-
	choose_var(Vs, Var, R),
	ground(Var),!,
	choose_my_var(R, V). 
choose_my_var(Vs, V) :-
	choose_var(Vs, V, _).

commit_v(Vs, r_s(W, d(Done, LDS))) :-
	choose_my_var(Vs, V),
	choose_value(V, Value),
	V = Value,
	insert_last(Done, '='(Value), NewDone),
	calculate_LDS(LDS, NewLDS),
	include_r_s(NewLDS, W, r_s(W, d(NewDone, NewLDS))),
	fail.
commit_v(_,_).

calculate_LDS(LDS, NewLDS) :-
	(LDS > 0 ->
	 NewLDS is LDS - 1
	;
	 NewLDS = 0).

recall(W) :- 
	retract_fact(sh(W, Flag)),
%	display(Flag),nl,
	(Flag = ok ->
	 retract_fact(msg(W, share)),
	 asserta_fact(sh(W, ok)),
	 asserta_fact(msg(W, share))
%	 display('Updated share queue'),nl
	;
	 assertz_fact(sh(W, ko))).

work_to_share(W) :-
	findall(r_s(W,d(D,L)), (r_s(W,d(D,L)), \+(D=bottom)), AvWork),
	length(AvWork, N),
	N > 3,
	number_of_workers(NumW),
	Total is N / (NumW + 1),
	TotalInt is integer(Total),
	TotalInt > 0,
%	reverse(AvWork, RevAvWork),
%	collect_nodes(RevAvWork, TotalInt, L), 
	last(AvWork, r_s(W, d(Done, LDS))), retract_fact(r_s(W, d(Done, LDS))),L=[d(Done,LDS)],
	m2manager(share(L)).
work_to_share(_) :-
	m2manager(share(nowork)).
%	m2manager(share(nowork), [], '')).

collect_nodes(AvWork, Total, [d(Done, LDS)|L]) :-
	Total > 0, !,
%	display(totalint(Total)), nl,
	AvWork=[r_s(W, d(Done, LDS))|R],
 	retract_fact(msg(W, explore)),
 	retract_fact(r_s(W, d(Done, LDS))),
%	display(work_to_share(Done, LDS)),nl,
	NewTotal is Total - 1,
	collect_nodes(R, NewTotal, L).
collect_nodes(_, _, []) .

request_work(W) :-
	retract_fact(store(W,_,_,_)),
	local_address(Address),
	m2manager(find(W, Address)).

m2manager(Message) :-
%  	display(preparing_to_send(Message)),nl,
 	(Message = collect(_,_) ->
	 asserta_fact(m(Message))
	;
	 assertz_fact(m(Message))
	).
%	do_m2manager(Message).

do_m2manager(Message) :-
	display(actually_sending(Message)),nl,
	manager_address(Manager),
	manager_process_message(Message) @ Manager,!.
do_m2manager(_) :- display('VAYA POR DIOS'), nl.

eval_store([],_) :- !.
eval_store(Done, [V|Vs]) :- 
	eval_var(V, Done, RDone),
	eval_store(RDone, Vs).

eval_var(V, Done, Done) :- 
	ground(V), !.
eval_var(_, [], []) :- !.
eval_var(V, [Done|Ds], RDone) :- 
	apply_done(V, Done),
	eval_var(V, Ds, RDone).

apply_done(V, '='(Point)) :- !,V = Point.
apply_done(V, '>'(Point)) :- !,V .>. Point.
apply_done(V, '<'(Point)) :- V .<. Point.
