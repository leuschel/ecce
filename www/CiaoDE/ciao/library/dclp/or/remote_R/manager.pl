:- module(manager, [
	main/0,
	dlabeling/3,
	manager_process_message/1,
	retrieve_solution/1], 
	[actmods]).

:- use_module(library(read)).
:- use_module(library(system)).
:- use_module(library(streams)).
:- use_module(library(aggregates)).
:- use_module(library(dynamic)).
:- use_module(library(concurrency)).
:- use_module(library(lists)).

:- use_package(remote).

:- use_module(library('actmods/webbased_locate')).
:- use_active_module(binder, [address/2, add_address/2, remove_address/1, 
	                       get_all_agencies/1]). 

:- use_module(library(prolog_sys), [statistics/2]).
:- use_module(library(format)).

% Number of workers and agencies
:- data n_of_workers/1.
:- data n_of_agencies/1.
% Solutions store
% :- multifile s/1.
:- data s/1.
% Number of solutions found
:- data n_s/1.
% Work share has arrived
:- concurrent work2share/1.
% Manager representation of the worker: Id, Status, Agency
:- concurrent w/3.

%Receive:
:- concurrent find/2.
:- concurrent collect/2.
:- concurrent share/1.
%Send:
:- concurrent m/2.

:- multifile is_boundto/1.
:- concurrent is_boundto/1.

:- concurrent i_am_done/1.

main :- 
	eng_call(register_to_binder, create, create),
	serve_random_port.
register_to_binder :-
	startup,
	current_host(Host),
	pause(1),
	is_boundto(Port),
	Address=a(Host, Port),
	add_address(manager, Address),
	current_fact(n_of_workers(N)),
	get_all_agencies(AgenciesList),
	length(AgenciesList, M),
	asserta_fact(n_of_agencies(M)),
	NumW is M * N,
	init_all_workers(AgenciesList, N, NumW, Address),
	findall(w(W,A,S),w(W,A,S),Ws),
	display(workers(Ws)),nl,
	display('System enabled... waiting for operations'), nl,
	eng_call(manager_process_find, create, create),
 	eng_call(manager_process_share, create, create),
  	eng_call(manager_process_collect, create, create),
	eng_call(send_message, create, create).

% LDS determines the value for Limited Discrepancy Search
dlabeling(Store, Politics, LDS) :-
	retractall(s(_)),
	retractall(n_s(_)),
	asserta_fact(n_s(0)),
	statistics(walltime,_),
	display('Launching search: '),
	choose_worker(w(W, Agency, idle)),
	display(explore(W, Store)), nl,
	set_busy(W, Agency),
	m2worker(e(W, Store, [d([], LDS)], Politics), Agency),
	rest_on_dole(Store, Politics),
	display('Exploration started!'), nl.

retrieve_solution(Result) :-
	retract_fact(i_am_done(Result)).

send_message :-
	retract_fact(m(M,A)),
	do_m2worker(M,A),
	send_message.

manager_process_message(M) :- 
	display(received_message(M)), nl,
	assertz_fact(M),
	display(queued_message(M)), nl.

manager_process_collect:-
	retract_fact(collect(Sol, Politics)),
 	display(s(Sol)), nl,
	asserta_fact(s(Sol)),
	process_collect(Politics),
	manager_process_collect.

manager_process_find :-
	retract_fact(find(W, Agency)),
	display(find(W)), nl,
	set_idle(W, Agency),
	eng_call(process_find(W, Agency), create, create),
	manager_process_find.

manager_process_share :-
	retract_fact(share(L)),
	display(share(L)),nl,
	assertz_fact(work2share(L)),
	manager_process_share.

process_find(W, Agency) :-
	display('Work search thread started'),nl,
	findall(w(BusyW, H, busy), w(BusyW, H, busy) , BusyWs),
	display('Possible employers'(BusyWs)),nl,
	do_process_find(BusyWs, W, Agency).

do_process_find([], W, Agency) :-
	assertz_fact(find(W, Agency)).
do_process_find([w(BusyW,H,_)|Bs], W, Agency) :- 
	m2worker(share(BusyW), H),
	display('Work_request_sent_to'(BusyW, H)),nl,
	retract_fact(work2share(Work)),
 	display('Response_from_worker'(BusyW, Work)),nl,
	(Work = nowork -> 
	 do_process_find(Bs, W, Agency)
	;
 	 m2worker(r_e(W, Work), Agency),
 	 set_busy(W, Agency)).

process_collect(one) :- !,
	retract_fact(s(Sol)),
	asserta_fact(i_am_done(Sol)),
	stop_all.
process_collect(all) :- !,
	n_of_agencies(M),
	n_of_workers(N),
	Total is N * M,
	findall(w(W,A,idle), w(W,A,idle), L),
	length(L, IdleN),
	display(collect_all(IdleN)),nl,
 	(Total is IdleN - 1 ->
	 findall(s(Sol), s(Sol), LSols),
	 asserta_fact(i_am_done(LSols)),
	 stop_all
 	;
	 true).
process_collect(N) :-
	number(N), !,
	display(collecting),nl,
	retract_fact(n_s(M1)),
	display(n_s(M1)),nl,
	M is M1 + 1,
	display(collect_n(M)),nl,
	(M is N + 1 ->
	 findall(s(Sol), s(Sol), LSols),
	 asserta_fact(i_am_done(LSols)),
	 stop_all
	;
	 asserta_fact(n_s(M))),
	display('Collection finished'), nl.
process_collect(_).

stop_all :-
	statistics(walltime,[_, Time]),
	format("Used ~d milliseconds~n", Time),
	retract_fact_nb(w(W, Agency, busy)),
	asserta_fact(w(W, Agency, idle)),
	m2worker(stop(W), Agency),
	display(stop(W, Agency)),nl,
	fail.
stop_all.
	
choose_worker(w(W, Agency, idle)) :-
	current_fact(w(W, Agency, idle)), !.

rest_on_dole(S,P) :-
	findall(w(W,Agency,idle), w(W,Agency,idle), L),
	display(rest_on_dole(L)),nl,
	do_rest_on_dole(L, S, P).

do_rest_on_dole([w(void, void,idle)],_,_).
do_rest_on_dole([w(W, Agency, idle)|Ws], S, P) :-
	m2worker(e(W, S, [], P), Agency), %To init. the store in the agency
	manager_process_message(find(W, Agency)),
	do_rest_on_dole(Ws, S, P).

set_busy(W, Agency) :-
	retract_fact(w(W, Agency, _)),
	asserta_fact(w(W, Agency, busy)).
set_idle(W, Agency) :-
	retract_fact(w(W, Agency, _)),
	asserta_fact(w(W, Agency, idle)).

init_all_workers([], _, _, _) :- assertz_fact(w(void, void, idle)).
init_all_workers([Agency|As], N, NumW, LocalAddress) :-
	set_manager(LocalAddress) @ Agency,
	init_workers(N, WorkersIds) @ Agency,
	set_num_workers(NumW) @ Agency,
	read_workers(WorkersIds, Agency),
	init_all_workers(As, N, NumW, LocalAddress).

read_workers([],_).
read_workers([Id|Ws], Agency) :-
	asserta_fact(w(Id, Agency, idle)),
	read_workers(Ws, Agency).


m2worker(Message, Address) :-
	assertz_fact(m(Message, Address)).
%	do_m2worker(Message, Address).
	
do_m2worker(Message, Address) :-
	agent_process_message(Message) @ Address.
do_m2worker(_,_) :- display('VAYA POR DIOS'), nl.

startup :- 
	current_executable(E),
	atom_concat(E,'_ini.pl',File),
	open_input(File, IO), !,
	repeat,
	   read(T),
	   process(T),
	T == end_of_file, !,
	close_input(IO),
	assertz_fact(s(void)).

process(end_of_file) :- !.
process(n_of_workers(N)) :- !,
	asserta_fact(n_of_workers(N)).
