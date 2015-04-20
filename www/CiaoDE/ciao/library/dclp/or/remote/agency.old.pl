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
:- concurrent r_s/5.
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
% Original store
:- data store/1.

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
explore_list(W, [e(Vs, Done, Politics, LDS)|L]) :- !,
%	display('Agency serving message: '),
	display(explore(W, Vs, Done, Politics, LDS)),nl,
	asserta_fact(r_s(W, Vs, Done, Politics, LDS)),
	asserta_fact(msg(W, explore)),
	explore_list(W, L).

do_agent_process_message(e(W, L)):-
	explore_list(W, L), !.
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
	assertz_fact(r_s(N, [], [], '', 0)),
	assertz_fact(sh(N, ko)),
	N1 is N - 1,
	init_workers(N1, Ws).
init_workers(0, []) .

worker(N) :- 
%	eng_goal_id(W),
% 	display(agency_worker(N)), nl,
	retract_fact(msg(N, Message)),
	display(proc_msg(N, Message)), nl,
	process_message(Message, N),
	worker(N).

process_message(explore, W) :- 
	retract_fact(r_s(W, Vs, Done, Politics, LDS)), !,
	display(exploring(r_s(W, Vs, Done, Politics, LDS))),nl,
	(Vs = [] ->
	 request_work(W)
	;
	 (ground(Vs) ->
%	 display('I_Found_a_solution'(W,Vs,Done)),nl,
	  m2manager(collect(Vs, Politics))
	 ;
	  commit_c(r_s(W, Vs, Done, Politics, LDS)),
%	  display('Commit done'),nl,
	  recall(W)),
%	  display('Recall done'),nl),
	 current_fact(r_s(W,VLeft,_,_,_)),
	 (VLeft=[]->
	  request_work(W)
	 ;
	  true)).
process_message(share, W) :- !,
	retract_fact(sh(W, _)),
	display(share(W)),nl,
	eng_call(work_to_share(W), create, create).
process_message(stop, W) :- !,
	display(stopping(W)),nl,
	retractall(r_s(W,_,_,_,_)),
	retractall(msg(W,_)).
process_message(_,_).

commit_c(r_s(W, Vs, Done, Politics, LDS)) :-
	choose_my_var(Vs, V),
%	display(choose_var(V)),nl,
	pitm(V, Point),
%	display(pitm(Point)),nl,
	calculate_LDS(LDS, NewLDS),
	add_constraint(V, Point, r_s(W, Vs, Done, Politics, NewLDS)),
%	display('Added new constraint'),nl,
	fail.
commit_c(_).

add_constraint(V, Point, r_s(W, Vs, Done, Politics, LDS)) :-
	V .>. Point,
	display('>'(V,Point)),nl,
	include_r_s(LDS, W, r_s(W, Vs, ['>'(Point)|Done], Politics, LDS)).
add_constraint(V, Point, r_s(W, Vs, Done, Politics, LDS)) :-
	V .<. Point,
	display('<'(V,Point)),nl,
	include_r_s(LDS, W, r_s(W, Vs, ['<'(Point)|Done], Politics, LDS)).
add_constraint(V, Point, r_s(W, Vs, Done, Politics, LDS)) :-
	V = Point,
	display('='(V,Point)),nl,
	include_r_s(LDS, W, r_s(W, Vs, ['='(Point)|Done], Politics, LDS)).

include_r_s(0, W, R_S) :- !,
	asserta_fact(R_S),
	display(asserted_r_s(R_S)),nl,
	asserta_fact(msg(W, explore)).
include_r_s(LDS, W, R_S) :- 
	LDS > 0,
	Bottom = r_s(W, [], [], '', 0),
%	display(lds(LDS)),nl,
	retract_fact(Bottom),
	assertz_fact(R_S),
	assertz_fact(Bottom),
	asserta_fact(msg(W, explore)), !.
%	display('Asserted_new_explore_msg'),nl.

choose_my_var(Vs, V) :-
	choose_var(Vs, Var, R),
	ground(Var), !,
	choose_my_var(R, V). 
choose_my_var(Vs, V) :- !,
	choose_var(Vs, V, _).

commit_v(r_s(W, Vs, Done, Politics, LDS)) :-
	choose_my_var(Vs, V),
	choose_value(V, Value),
	V = Value,
	NewDone = [Value|Done],
	calculate_LDS(LDS, NewLDS),
	include_r_s(NewLDS, W, r_s(W, Vs, NewDone, Politics, NewLDS)),
	fail.
commit_v(_).

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
	findall(r_s(W,Work,D,P,L), (r_s(W,Work,D,P,L), \+(Work=[])), AvWork),
%	display(a(AvWork)),nl,
	length(AvWork, N),
	N > 3,
%	number_of_workers(NumW),
%	Total is N / NumW,
%	TotalInt is integer(Total),
	reverse(AvWork, RevAvWork),
	
%	display(total(TotalInt)),nl,
%	collect_nodes(RevAvWork, TotalInt, L), 
	collect_nodes(RevAvWork, 1, L), 
	m2manager(share(L)).
work_to_share(_) :-
	m2manager(share(nowork)).
%	m2manager(share(nowork), [], '')).

collect_nodes(AvWork, Total, [e(Work2Share,Done,Politics,LDS)|L]) :-
	Total > 0, !,
%	display(totalint(Total)), nl,
	AvWork=[r_s(W,_, Done, Politics, LDS)|R],
	retract_fact(r_s(W, Work2Share, Done, Politics, LDS)),
%	display(work_to_share(Work2Share, Done, Politics, LDS)),nl,
	retract_fact(msg(W, explore)),
	NewTotal is Total - 1,
	collect_nodes(R, NewTotal, L).
collect_nodes(_, _, []) .

request_work(W) :-
	 local_address(Address),
	 m2manager(find(W, Address)).

m2manager(Message) :-
% 	display(preparing_to_send(Message)),nl,
 	assertz_fact(m(Message)).
%	do_m2manager(Message).

do_m2manager(Message) :-
	display(actually_sending(Message)),nl,
	manager_address(Manager),
	manager_process_message(Message) @ Manager.
do_m2manager(_) :- display('VAYA POR DIOS'), nl.
