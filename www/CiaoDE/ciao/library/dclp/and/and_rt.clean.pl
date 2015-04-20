:- module(and_rt, [d_labeling/1, store/2, '@'/2], []).

:- use_module(library(concurrency)).
:- use_module(library(lists)).
:- use_module(library(conc_aggregates)).
:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(streams)).
:- use_module(library(format)).
%:- use_module(library(system)).

:- data source/1.
:- data store/3.
:- data l_store/3.
:- data p_store/3.
:- concurrent msg/3.
% Agent id. and assignments list.
:- data v/2.
% Agent id., no_good sender, and no_good.
:- data no_good/3.
% Agent id. and a list with its assignments.
:- data a/2.
% Pending ok? to be sent. Due to unsolved no_goods: 
:- data pending_ok/2.
% Agent id. and a list with its assignments.
:- data tmp_a/2.
% Agent's local store
:- data local_store/2.
% assignments look like this a(1, [x(x1, X1, Val1),...,x(xn, Xn, Valn)])
% old assignments
:- data old_assignment/2.
% Adapted Dijsktra-Scholten for building a spanning tree during propagation. 
% Agent, Parent
:- data parent/2.
% Messages unacknowledged
:- data ackleft/3.
%:- concurrent send/1.
% Phase
:- data phase/2.
% Chandy-Lamport: Agent Id, Stage (four counters algorithms)
:- data marker/1.
:- data bkup_marker/1.
:- data marker_counter/2.
:- data waiting_for_completeness/2.
:- concurrent view_is_complete/3.

% Begin_labeling synchronization Id, RId
:- data waiting_for_labs/2.

% Pending of results collection:
:- data waiting_for_collection/1.
:- data b/2. % Bag of solution assignments
:- concurrent collection_complete/1.

% For statistics only. Bag to store info (msg, sender, receiver, phase) about 
% the packages received:
:- data stats/4.

d_labeling(L) :-
	(current_fact('$entailments_gathered') ->
	 display(d_lab2), nl,
	 display('$entailments_gathered'), nl, 
	 send_flush,
	 display(d_lab3), nl
	;
	 display(d_lab4), nl,
	 set_source,
	 display(d_lab5), nl,
	 get_ids(L, LIds), 
	 display(d_lab6), nl,
	 gather_entails(LIds),
	 display(d_lab7), nl,
	 source(S),
	 display(d_lab8), nl,
	 check_not_empty_store(S),
	 display(d_lab9), nl,
	 retractall(vars_dic(_,_,_,_)),
	 display(d_lab10), nl,
	 asserta_fact('$entailments_gathered'),
	 display(d_lab11), nl,
	 init_agents,
	 display(d_lab12), nl
	),
	display(kk),nl,
	wait_for_solution(L, R),
	display(kk2),nl,
	(R = fail ->
	 display(d_lab13), nl,
	 !, fail
	;
	 display(d_lab14), nl,	  
	 true),
	display(d_lab15), nl,
	eng_status,
	 display(d_lab16), nl.
d_labeling(L) :-
	display(d_lab_bis), nl,
	do_d_labeling(L),
	eng_status.

do_d_labeling(L) :-
	display('Waiting for more solutions'(L)), nl,
	induce_bkt,
	wait_for_solution(L, R),
	(R = fail ->
	 !, fail
	;
	 true).
do_d_labeling(L) :-
	do_d_labeling(L).

wait_for_solution(L, Return) :-
       retract_fact(collection_complete(Return)), !,
       display(Return), nl,
       (Return = true ->
	 display('Comitting assignments'), nl,
	 commit_assignments(L),
	 display('Assignments committed'), nl
       ;
	 true
       ).

commit_assignments([]).
commit_assignments([V|Vs]) :-
	display(committing(V)), nl,
	(get_attribute(V, '$dl'(V, Id)) ->
	 display(att(Id)),nl,
	 detach_attribute(V),
	 display('attribute detached'), nl,
	 retract_fact(b(Id, Value)),
	 display('b retracted'(Id, Value)), nl,
	 V = Value
	;
	 true),
	commit_assignments(Vs).

induce_bkt :-
	display('inducing backtracking'), nl,
	get_lower_priority_agent(S, Ag),
	display('lower priority agent'(Ag)), nl,
	send_message(msg(Ag, S, enforce_backtrack)),
	restart_termination_detection(S).

restart_termination_detection(S) :-
	send_message(msg(S, _, restart_termination_detection)).
 
init_agents :-
	store(NId, Vs, Rs),
	append(Vs, Rs, Ts), 
	eng_call(agent(NId, Ts), create, create),
	fail.
init_agents.

initialize_agent(NId, Vs) :-
	source(S),
	asserta_fact(phase(NId, prop)),
	asserta_fact(marker_counter(NId, 0)),
	set_initial_ack(NId, Ack_st),
	asserta_fact(ackleft(NId, 0, Ack_st)),
	store(NId, LVs,_),
	initialize_prop_dependence(NId, LVs),
	(S = NId ->
	 enable_collection,
	 asserta_fact(parent(NId, NId))
	;
	 true),
	(call_entailment(NId, L1) ->
	 list(L1),!,
	 filter(L1, Vs, _, L),
	 show_ranges2(L),
	 asserta_fact(a(NId, L)),
	 display(local_store(NId, L)), nl,flush_output,
	 asserta_fact(local_store(NId, L)),
	 send_ok(NId, Vs, L)
	;
	 send_message(msg(S, NId, collect(fail)))
	).

enable_collection :-
	retractall(waiting_for_collection(_)),
	do_enable_collection.
do_enable_collection :-
	store(Id,_,_),
	asserta_fact(waiting_for_collection(Id)),
	fail.
do_enable_collection.

initialize_prop_dependence(N, Vs) :-
	store(Id, _IdVs, IdRVs),
	Id > N,
 	intersection(IdRVs, Vs, I),
 	(I = [] ->
 	 true
 	;
	 asserta_fact(waiting_for_labs(N, Id))),
	fail.
initialize_prop_dependence(_,_) .

set_initial_ack(NId, Ack_st) :-
	store(N,_,_),
	N > NId,
% 0 in case NId is the lower priority agent	
	Ack_st = 0.
set_initial_ack(_, 1).

get_lower_priority_agent(S, Ag) :-
	source(S),
	do_get_lower_priority_agent(S, Ag).

do_get_lower_priority_agent(S, Ag) :-
	current_fact(store(N,_,_)),
	N > S, !,
	do_get_lower_priority_agent(N, Ag).
do_get_lower_priority_agent(Ag, Ag).

filter([],_,[],[]).
filter([E|Es], Vs, [Var|Vars], [NewE|NEs]) :-
	E = x(F,_,Var),
	member(F, Vs),!,
	display(in(F,Vs)),nl,
	NewE = x(F, Var, Var),
	filter(Es, Vs, Vars, NEs).
filter([_|Es], Vs, Vars, NEs) :-
 	filter(Es, Vs, Vars, NEs).

agent(N, Vs) :- 
   	number_codes(N, C),
  	atom_codes(A, C),
  	atom_concat(['./',A], F),
  	open_output(F, S),
	initialize_agent(N, Vs),
	do_agent(N),
	close_output(S).

do_agent(N) :-
	display(do_agent(N)),nl,flush_output,
	retract_fact(msg(N, K, Message)),
	store_stats(Message, N, K),
	display(received(msg(N, K, Message))),nl,flush_output,
	(current_fact(phase(N,prop)) -> 
	 process_ack(msg(N, K, Message))
	;
	 true),
	process_marker(Message, N, K),
	process_message(Message, N, K),
	do_agent(N).

process_marker(prop_ended,_,_) :- !.
process_marker(begin_labeling,_,_) :- !.
process_marker(show_stats,_,_) :- !.
process_marker(harvest,_,_) :- !.
process_marker(collect(_),_,_) :- !.
process_marker(flush,_,_) :- !.
process_marker(maybe_wake_up,_,_) :- !.
process_marker(enforce_backtrack,_,_) :- !. 
process_marker(restart_termination_detection,_,_) :- !.
process_marker(ack(_), N,_) :- 
	current_fact(phase(N, lab)), !.
process_marker(M, N, K) :-
	source(N),!,
	do_process_marker(N, K, M).
process_marker(_,_,_).

do_process_marker(N, K, M) :-
	current_fact(marker_counter(N, MC)),
	display(current_marker_counter(MC)),nl,
  	findall(marker(Id), marker(Id), L),
  	display(pending_markers(L)),nl,
	display(M),nl,
	(M = marker(C) ->
	 (C = MC ->
	  retract_fact(marker(K)),
	  (current_fact(marker(_)) ->
	   true
	  ;
	   display('Moving...'),nl,
	   current_fact(phase(N, Phase)),
	   (Phase = lab ->
	    change_state(void, N)
	   ;
	    display('Prop finished?'),nl,
	    change_state(prop, N)
	   )
	  )
	 ;
	  display(discarded_marker(C)) ,nl
	 )
	;
	 display('Execution not terminated'),nl,
	 backup_markers,
	 NewC is MC + 1,
	 display(marker_counter_updated(MC, NewC)),nl,
	 retract_fact(marker_counter(N,_)),
	 asserta_fact(marker_counter(N, NewC))
	).
do_process_marker(_,_,_).

backup_markers :-
	retractall(bkup_marker(_)),!,
	do_backup_markers.

do_backup_markers :-
	retract_fact(marker(M)),
	asserta_fact(bkup_marker(M)),
	fail.
do_backup_markers :-
	retractall(marker(_)).

r_backup_markers :-
	retract_fact(bkup_marker(M)),
	asserta_fact(marker(M)),
	fail.
r_backup_markers :-
	retractall(bkup_marker(_)).

process_ack(msg(_,_, ack(_))) :- !.
process_ack(msg(_,_, marker(_))) :- !.
process_ack(msg(_,_, prop_ended)) :- !.
process_ack(msg(_,_, begin_labeling)) :- !.
process_ack(msg(_,_, show_stats)) :- !.
process_ack(msg(_,_, harvest)) :- !.
process_ack(msg(_,_, collect(_))) :- !.
process_ack(msg(_,_, flush)) :- !.
process_ack(msg(_,_, maybe_wake_up)) :- !. 
process_ack(msg(_,_, enforce_backtrack)) :- !. 
process_ack(msg(_,_, restart_termination_detection)) :- !.
process_ack(msg(N, K,_)) :- 
	source(N),!,
	send_message(msg(K, 1, ack(void))).
process_ack(msg(N, K, M)) :- 
	display(ack(M)),
	nl,flush_output,
	(retract_fact(parent(N, P)) ->
	 display(parent(N,P)),nl,flush_output,
	 asserta_fact(parent(N,P)) 
	;
	 display(newParent),nl,flush_output,
	 asserta_fact(parent(N, K)),
	 display(asserted(parent(N, K))),
	 nl,flush_output).

process_message('ok?'(a(S)), N, K) :- !,
	display('ok?'(a(K, S))),nl,flush_output,
	(current_fact(v(N, a(K,_))) ->
	 retractall(v(N, a(K,_))),
	 display(a),nl,flush_output
	;
	 display(b),nl
	),
	asserta_fact(v(N, a(K, S))),
	check_agent_view(N, K),
	wake_termin_detection(N, K).

process_message(no_good(No_good), N, K) :-
	display(no_good_clause_1(No_good)),nl,flush_output,
  	stored(N, No_good),!, %We are already processing the no_good
	phase(N, Phase),
	asserta_fact(stats(N, K, neglected_no_good, Phase)),
	restore_markers(N),
	wake_termin_detection(N, K).
% 	current_fact(a(N,L)),
% 	send_ok_one(N, K, L).
	
process_message(no_good(No_good), N, K) :- 
% El no_good es el agent_view del agente en que se dio
% Ej: No_good=[x(x1, X1,V1), x(x2, X2,V2),...]
	current_fact(a(N, A)),
	display(a), nl,flush_output,
	display(b), nl,flush_output,
	fetch_assignments(No_good, LVA),
	display(c), nl,flush_output,
	append(LVA, A, Cs),
	display(d),nl,flush_output,
	display('consistent?'(Cs)), nl,flush_output,
	consistent_vars(Cs),
%	consistent_view(No_good, N),
	display(consistent(A, LVA)), nl,
	asserta_fact(no_good(N, A, No_good)),
	store(N,Local,LinkedVars),
	display(e), nl,flush_output,
	check_all_linked(No_good, N, LinkedVars),
	display(f), nl,flush_output,
	local_backtrack(N, K, Local, A, LVA),
	display(g), nl.
process_message(begin_labeling, N,_) :-
	(current_fact(waiting_for_labs(N,_)) ->
	 true
	;
	 display('No remaining prop_ended'), nl,
	 store(N,_, RVs),
	 send_up_prop_ended(N, RVs),
	 do_begin_labeling(N)).
process_message(ack(A), N, K) :-
	current_fact(phase(N, prop)),
	retract_fact(ackleft(N, AckLeft, St)),
	AckLeft >= 0,
	display(ack(a(AckLeft))),nl,flush_output,
	AckLeft1 is AckLeft - 1,
	asserta_fact(ackleft(N, AckLeft1, St)),
	add_propagation(A, N, L, K),!,
	display(added_propagation(A, N, L, K)),nl,flush_output,
	current_fact(ackleft(N, NewAckLeft, St)),
	display(newackleft(NewAckLeft)),nl,flush_output,
	(NewAckLeft = 0 -> 
	 (source(N) -> 
	  true
	 ;
	  (quiescent(N) ->
	   retract_fact(parent(N, P)) ,
 	   send_message(msg(P, N, ack(L)))
  	  ;
 	   true	   
 	  )
	 )
	;
	 true). 
process_message(marker(C), N, K) :- !,
	(source(N) ->
	 true
	;
	 display(not_source),nl,
	 reply_marker(N, K, C)
	).
process_message(prop_ended, N, K) :-
	findall(waiting_for_labs(N, Kt), waiting_for_labs(N, Kt), LL),
	display(LL),nl,
	retract_fact(waiting_for_labs(N, K)),
	(current_fact(waiting_for_labs(N,KK)) ->
	 display(aun_queda(KK)),nl,
	 true
	;
	 display('No remaining prop_ended'), nl,
	 store(N,_, RVs),
	 send_up_prop_ended(N, RVs),
	 do_begin_labeling(N)).
process_message(enforce_backtrack, N, _K) :-  
	backtrack(N).
process_message(restart_termination_detection, N, _K) :- 
	enable_collection,
	send_all_marker(N).
process_message(maybe_wake_up, N, K) :- 
	wake_termin_detection(N, K).
process_message(show_stats, N, _K) :- 
	dump_stats(N).
process_message(harvest, N, _K) :- 
	display('Collection results...'), nl,
	collect_results(N).
process_message(flush, N, _K) :- 
	flush_structs(N),
	current_fact(p_store(N, Vs, Rs)),
	retract_fact(store(N,_,_)),
	asserta_fact(store(N, Vs, Rs)),
	append(Vs, Rs, Ts), 
	initialize_agent(N, Ts).
process_message(collect(fail), N, _K) :- !,
	source(N),
	display('Failing!'), nl,
	asserta_fact(collection_complete(fail)).
process_message(collect(L), N, K) :- 
	source(N),
	store_assignments(L),
	display(collected_from(K, L)), nl,
	retract_fact(waiting_for_collection(K)),
	(current_fact(waiting_for_collection(_)) ->
	 true
	;
	 asserta_fact(collection_complete(true))
	).
process_message(M,N,K) :- 
	M = no_good(_), !, 
	phase(N, Phase),
	asserta_fact(stats(N, K, neglected_no_good, Phase)),
	display('KKDLAVAK'(M,N,K)),nl,flush_output,
	restore_markers(N),
	wake_termin_detection(N, K).
process_message(_,_,_) .

restore_markers(N) :-
	(source(N) ->
	 retract_fact(marker_counter(N, C)),
	 display(marker_counter(N, C)),nl,
	 (C > 0 ->
	  NC is C - 1
	 ;
	  NC = C),
	 asserta_fact(marker_counter(N, NC)),
	 r_backup_markers
	;
	 true).

do_begin_labeling(N) :-
	display('Beginning actual labeling'), nl,
	update_stores(N),
	display('lab init phase 1'), nl,
	store(N, Vs, _),
	display('lab init phase 2'), nl,
	retractall(v(N,_)),
	display('lab init phase 3'), nl,
	retract_fact(a(N, L1)),
	filter(L1, Vs, _, L),
	extract_vars(L, LVs),
	copy_term(LVs, CLVs),
	compose_vars(L, LVs, CLVs, LocalStore),
   	retract_fact(local_store(N,_)),
   	asserta_fact(local_store(N, LocalStore)),
	labeling(CLVs),
 	retractall(old_assignment(N,_)),
 	asserta_fact(old_assignment(N, CLVs)),
	compose_vars(L, LVs, CLVs, NewL),
	display(newl(NewL)),nl,flush_output,
	asserta_fact(a(N, NewL)),
	retract_fact(phase(N,_)),
	asserta_fact(phase(N,lab)),
	send_ok(N, Vs, NewL),
	display('Actual labeling begun'), nl.

update_stores(N) :-
	current_fact(l_store(N, L, R)),
	retract_fact(store(N,_,_)),
	asserta_fact(store(N, L, R)).

send_up_prop_ended(N, Rvs) :-
	store(Id, Vs,_),
	Id < N,
 	intersection(Rvs, Vs, I),
 	(I = [] ->
 	 true
 	;
	 send_message(msg(Id, N, prop_ended))),
	fail.
send_up_prop_ended(_,_).
	
reply_marker(N, K, C) :-
	(quiescent(N) ->
	 display(quiescent),nl,
	 send_message(msg(K, N, marker(C)))
	;
      	 display(not_quiescent(N,K,C)) , nl,
	 asserta_fact(waiting_for_completeness(N, C)),
	 eng_call(quiescence_detector(N, K, C), create, create),
	 display(quiescence_detector_launched),nl
	).

change_state([],N) :- !,
	display('Global execution failed'), nl,flush_output,
	show_stats(N),
	asserta_fact(collection_complete(fail)).
change_state(void,N) :-
% No hace falta comprobar esto. Si a la fuente le han legado todos los
% markers es porque todos los agentes han acabado, por definicion (un
% agente que no tiene su vista completa espera a tenerla antes de mandar el
% marker correspondiente
% 	(quiescent(N) ->
% 	 display('Global execution finished'), nl,flush_output,
% 	 retractall(ackleft(N,_,_)),
% 	 harvest(N),
% 	 show_stats(N)
% 	;
% 	 true, display(not_quiescent),nl
% 	).
	display('Global execution finished'), nl,flush_output,
	retractall(ackleft(N,_,_)),
	harvest(N),
	show_stats(N).
change_state(_,N) :-
	(quiescent(N) ->
	 display('Moving from propagation to search'), nl,flush_output,
	 begin_labeling(N)
	;
	 true
	).
change_state(_,_).

harvest(N) :-
	store(NId,_,_),
	send_message(msg(NId, N, harvest)),
	fail.
harvest(_).

store_assignments([]) .
store_assignments([x(VId,_,Val)|As]) :-
	asserta_fact(b(VId, Val)),
	display(asserted(b(VId, Val))),nl,
	store_assignments(As).

begin_labeling(N) :-
	store(NId,_,_),
	send_message(msg(NId, N, begin_labeling)),
	fail.
begin_labeling(_).

add_propagation(void,_,void,_) :-!.
add_propagation(A,N,L,K) :-
	display(adding_propagation(A,N,L,K)), nl,
	retract_fact(a(N,L)),
	display(a(N,L)), nl,
	asserta_fact(tmp_a(N, L)),
	display(tmp_a), nl,
	(unify(L,A) ->
	 display(unify(L,A)), nl,
	 show_ranges2(L), nl,
	 display(unified),nl,flush_output,
	 retract_fact(tmp_a(N, TA)),
	 asserta_fact(a(N,L)),
	 (not_told(L, TA) ->
	  display(not_told),nl,
	  send_all_marker(N)
	 ;
	  display(told),nl,flush_output,
	  store(N,Vs,_),
	  (K = all ->
	   send_ok(N, Vs, L)
	  ;
	   send_ok_(N, K, Vs, L)
	  )
	 )
	;
	 (current_fact(phase(N, prop)) ->
	  source(S),
	  send_message(msg(S, N, collect(fail)))
	 ;
	  fail)
	).
add_propagation(_,_,[],_) .

unify([],_).
unify([X|Xs],Teller) :-
	unify_on(Teller, X),
	unify(Xs, Teller).

unify_on([],_).
unify_on([Y|Ys], X) :-
	X=x(F,_,_),
	Y=x(F,_,_),!,
	X=Y,
	unify_on(Ys, X).
unify_on([_|Ys], X) :-
	unify_on(Ys, X).

not_told([], []).
not_told([X|Xs], [Y|Ys]) :-
	X=x(F, VarX, _ValX),
	Y=x(F, VarY, _ValY),
	display(F),nl,
	retrieve_range(VarX, RangeX),
	retrieve_range(VarY, RangeY),
	RangeX = RangeY,
	not_told(Xs, Ys).

local_backtrack(N,_,Local, A, LVA) :-
	difference(LVA, A, NewLVA),
	change_assignments(N, A, NewLVA, AOut),
	display(changing_assignments),nl,flush_output,
	\+(AOut = A),	!,
	display(no_good_enforced_change(LVA, AOut)),nl,flush_output,
	retract_fact(a(N,_)),
	asserta_fact(a(N,AOut)),
	send_ok(N, Local, AOut).
local_backtrack(N,K,_,_,_) :-
	display(no_good_did_not_enforce_a_change),nl,flush_output,
% Not clear whether this is useful or not yet:
% 	current_fact(a(N,L)),
% 	send_ok_one(N, K, L),
% Let's try this instead:
	(source(N) ->
% This is wrong. Bactrack is over, so the execution has to fail.
% 	 display(sending_ok),nl,
% 	 send_ok_one(N, K, L)
	 change_state([],N)
	;
	 display(pending_ok),nl,
	 (current_fact(pending_ok(N, K)) ->
	  true
	 ;
	  asserta_fact(pending_ok(N, K)))),
	backtrack(N).

stored(N, No_good) :-
	display(s0),nl,flush_output,
	current_fact(a(N, CurrentA)),
	current_fact(no_good(N, StoredA, Stored_No_good)),
	append(CurrentA, StoredA, A),
	consistent_vars(A),
	display(s1),nl,flush_output,
	fetch_assignments(No_good, Nv),
	display(s2),nl,flush_output,
	fetch_assignments(Stored_No_good, SNv),
	display(s3),nl,flush_output,
	display(eq(Nv,SNv)),nl,flush_output,
	append(Nv,SNv,NGS),
	consistent_vars(NGS),
	display(s4),nl,flush_output.

check_all_linked([],_,_) .
check_all_linked([v(_,a(Id,_))|Vs], Id, LinkedVs) :- !,
	check_all_linked(Vs, Id, LinkedVs).
check_all_linked([v(_, a(VId, As))|Vs], Id, LinkedVs) :-
	VId < Id,
	(current_fact(v(Id, a(VId, _))) ->
	 true
	;
	 asserta_fact(v(Id, a(VId, As))),
	 check_linked(As, Id, LinkedVs)
	),
	check_all_linked(Vs, Id, LinkedVs) .

check_linked([],_,_).
check_linked([x(F,_,_)|As], Id, LinkedVs) :-
	(member(F, LinkedVs) ->
	 check_linked(As, Id, LinkedVs)
	;
	 display(adding(F, Id)),nl,flush_output,
	 retract_fact(store(Id, L, _)),
	 asserta_fact(store(Id, L, [F|LinkedVs])),
	 check_linked(As, Id, [F|LinkedVs])).

consistent_view([],_).
consistent_view([v(_, a(Id, A))|Vs], N) :-
	(current_fact(v(N, a(Id, SA))) ->
	 append(SA, A, TA),
	 consistent_vars(TA)
	;
	 true),
	 consistent_view(Vs, N).

consistent_vars([]).
consistent_vars([x(F,_,Val)|Rs]) :-
	store_consistent(Rs, F, Val),!,
	consistent_vars(Rs).

store_consistent([],_,_).
store_consistent([x(F,_,Val1)|Rs], F, Val) :- !, 
	(consistent(Val,Val1) ->
	 store_consistent(Rs, F, Val)
	;
	 fail).
store_consistent([_|Rs], F, Val) :-
 	store_consistent(Rs, F, Val).

consistent(Val,Val1) :-
	number(Val), 
	number(Val1), 
	Val = Val1, !.
consistent(Val,_) :-
	var(Val), !.
consistent(_,Val1) :-
	var(Val1), !.
	 
check_agent_view(N, K) :-
	display(checking_agent_view(N, K)),nl,flush_output,
	findall(v(N, A), v(N, A), Vs),
	display(Vs),nl,flush_output,
	fetch_assignments(Vs, Rs),
	display(fetched), nl,
	consistent_vars(Rs),
	display(consistent),nl,
	current_fact(a(N, L)),
	display('L exists'), nl,
	display(local_assignment(a(N, L))),nl,flush_output,
	(check_consistent(N, L, Rs, Phase) ->
	 (Phase = lab ->
% Solving pending ok?s:
	  solve_pending_oks(N, L)
	 ;	
	  retract_fact(ackleft(N, AckLeft,St)),
	  display(k(St)),nl,
	  display(ackleft_cv(AckLeft)),nl,flush_output,
	  (St = 0 ->
	   NewSt = 1
	  ;	      
	   NewSt = St,
	   send_message(msg(K, N, ack(L)))),
	   asserta_fact(ackleft(N, AckLeft, NewSt)),
	  (AckLeft = 0, retract_fact(parent(N, P)), \+(P==K) -> 
	   send_message(msg(P, N, ack(L)))
         ;
	   true)
        )
	;
	 (Phase = lab ->
	  change_assignments(N, L, Rs, Out),
	  display(a),nl,flush_output,
	  retract_fact(a(N,_)),
	  display(b),nl,flush_output,
	  asserta_fact(a(N,Out)),
	  display(c),nl,flush_output,
	  store(N, VNs,_),
	  display(d),nl,flush_output,
	  send_ok(N, VNs, Out),
	  display(e),nl
	 ;
	  current_fact(parent(N, P)),
	  send_message(msg(P, N, ack([])))
	 )
	).
check_agent_view(N,_) :-
 	backtrack(N).	 

solve_pending_oks(N, L) :-
	retract_fact(pending_ok(N, K1)),
	send_ok_one(N, K1, L).
solve_pending_oks(_,_).

fetch_assignments([], []).
fetch_assignments([v(_, a(_,A))|Vs], Rs) :-
	fetch_assignments(Vs, R),
	append(A, R, Rs).

check_consistent(N, L, Rs, Phase) :-
	display(a), nl,
	append(L, Rs, Cs),
	display('consistent?'), nl,
	consistent_vars(Cs),
	display('consistent!'), nl,
	(extract_vals(Cs,_) -> 
	 display(vals_extracted), nl,
	 call_entailment(N, Cs),
	 display(b2),nl,flush_output,
	 extract_vals(L,LVals),
	 display(b3),nl,flush_output,
	 Phase = lab,
	 display(LVals),nl,flush_output,
	 (current_fact(old_assignment(N, LVals)) ->
	  true
	 ;
	  assertz_fact(old_assignment(N, LVals)),
	  display(asserted_old_assignment),nl,flush_output)
	;
	 Phase = prop,
	 add_propagation(Rs,N,_,all)
	).

change_assignments(N, L, Rs, NewL) :-
	display(change_assignments),nl,
	current_fact(local_store(N, L1)),
	display(current_local_store), nl,
	extract_vars(L1, CVs),
	display(l1_extracted),nl,
	extract_vars(L, Vs),
	display(l_extracted),nl, !,
	labeling(CVs),
	display(lab(CVs)), nl,
	(current_fact(old_assignment(N, CVs)) -> 
	 fail
	; 
	 true),
	compose_vars(L, Vs, CVs, NewL),
	display(composed), nl, flush_output,
	check_consistent(N, NewL, Rs,_),
	display(new_local_assignment(NewL)),nl,flush_output.

extract_vars([],[]).
extract_vars([x(_,V,_)|As], [V|Vs]) :-
	extract_vars(As, Vs).

extract_vals([],[]).
extract_vals([x(_,_,Value)|As],[Value|Vs]) :-
	number(Value),!,
	extract_vals(As, Vs).

compose_vars([], [], [], []) .
compose_vars([x(F,_,_)|As], [V|Vs], [Val|Vals], [NewA|Ns]) :-
	NewA = x(F, V, Val),
	compose_vars(As, Vs, Vals, Ns).

send_ok(N, VNs, Out) :-
	store(Id,_,RIds),
	Id > N,
	intersection(VNs, RIds, I),
	(I = [] ->
	 true
	;
	 send_message(msg(Id, N, 'ok?'(a(Out))))),
	fail.
send_ok(N,_,_) :-
	send_all_marker(N).

send_ok_(N, K, VNs, Out) :-
	store(Id,_,RIds),
	Id > N,
	\+(Id = K),
	intersection(VNs, RIds, I),
	(I = [] ->
	 true
	;
	 send_message(msg(Id, N, 'ok?'(a(Out))))),
	fail.
send_ok_(N,_,_,_) :-
	 send_all_marker(N).

send_ok_one(N, K, Out) :-
	send_message(msg(K, N, 'ok?'(a(Out)))),
	send_all_marker(N).

send_all_marker(N) :-
	source(N),
	current_fact(marker_counter(N, C)),
 	store(Id,_,_RIds),
	\+ (Id = N),
	asserta_fact(marker(Id)),
	send_message(msg(Id, N, marker(C))), %),
	fail.
send_all_marker(_). % :- 
%  	findall(marker(Id), marker(Id), L),
%  	display(L), nl.

send_flush :-
	store(N,_,_),
	send_message(msg(N,_,flush)),
	fail.
send_flush.

backtrack(N) :-
	display(backtracking(N)),nl,flush_output,
	store(N, _, R),
	findall(M, (store(M,_,_), M < N), Ms),
	display(st(Ms)),nl,flush_output,
	max(Ms, Lower, Rest),
	do_backtrack(Lower, Rest, N, R),
% Must remove all old_assignments as they may be valid upon receipt of new 
% values from higher priority agents
	retractall(old_assignment(N, _)).

do_backtrack(Lower, Rest, N, R) :-
	display(a(Lower,Rest)),nl,flush_output,
% I don't need to check different values for the same vars of the agent_view. 
% check_agent_view has already done it.
	store(Lower,Local,_),
	display(b),nl,flush_output,
	intersection(Local, R, I),
	display(c),nl,flush_output,
	(current_fact(v(N, a(Lower, PA))) ->
	 Els=PA
	;
	 Els=[]),
	display(d(I,Els)),nl,flush_output,
	((I = []; Els = [])->
	 display(e1),nl,flush_output,
	 max(Rest, NewLower, NewRest),
	 display(f1(NewLower, NewRest)),nl,flush_output,
	 do_backtrack(NewLower, NewRest, N, R),
	 display(g1),nl
	;
%Testing propagation of no_goods to higher priority agents
	 findall(v(N,a(Id, OA)), (v(N,a(Id, OA)), \+(Id=Lower)), Os),
	 display(e2),nl,flush_output,			 
	 retract_fact(v(N, a(Lower,A))),
	 display(f2),nl,flush_output,			 
	 compose_no_good(Os, Els, Vs),
	 display(g2),nl,flush_output,			 
	 TotalA=[v(N, a(Lower, A))|Vs],
	 display(h2),nl,flush_output,			 
	 send_message(msg(Lower, N, no_good(TotalA))),
	 display(i2),nl,flush_output).

compose_no_good([],_,[]).
compose_no_good([V|Vs], Els, NewVs) :-
	V = v(_,a(_, MA)),
	append(Els, MA, ElsMA),
	(consistent_vars(ElsMA)->
	 NewVs=[V|R]
	;
	 NewVs=R),
	compose_no_good(Vs, Els, R).
	
max([Max], Max, []).
max([M|Ms], Max, R) :-
	max(Ms, Max1, R1),
	(M > Max1 ->
	 Max = M,
	 R = Ms
	;
	 Max = Max1,
	 R = [M|R1]).

quiescence_detector(N, K, C) :-
	retract_fact(view_is_complete(N, C, ThanksTo)),
	send_message(msg(K, N, marker(C))),
	(ThanksTo < N, phase(N,lab) ->
	 send_message(msg(ThanksTo, N, maybe_wake_up))
	;
	 true
	).

wake_termin_detection(N, ThanksTo) :-
       (waiting_for_completeness(N, C) ->
	(complete_view(N) ->
	 display(view_is_complete(N)), nl,
	 retractall(waiting_for_completeness(N, C)),
	 asserta_fact(view_is_complete(N, C, ThanksTo))
	;
	 true)
       ;
	true).

quiescent(N) :- 
	(current_fact_nb(msg(N,_,_)) ->
	 fail
	;
	 complete_view(N)). 
	
complete_view(N) :-
	store(N, _, Rs),
	display(store(N, Rs)), nl,
	findall(Id, (store(Id,_,_), Id < N), Ids),
	display(highers(Ids)), nl,
	get_presumed_view(Ids, Rs, PIds),
	display(presumed_view(PIds)), nl,
	do_complete_view(PIds, N).

get_presumed_view([], _, []).
get_presumed_view([Id|Ids], Rs, P) :-
	store(Id, VNs, _),
	intersection(VNs, Rs, I),
	(I = [] ->
	 P = RP
	;
	 P = [Id|RP]
	),
	get_presumed_view(Ids, Rs, RP).
	
do_complete_view([],_).
do_complete_view([Id|Ids], N) :-
	current_fact(v(N, a(Id,_))),
	do_complete_view(Ids, N).

call_entailment(AgentId, L) :-
	t_i(AgentId, EntailmentId),
	entails(EntailmentId, L).

send_message(Msg) :- 
	Msg = msg(_,N,M),
	(current_fact(phase(N, lab)); logistics(M)), !,
	display(sending(Msg)),nl,flush_output,
	assertz_fact(Msg),
	display(sent_message),nl,flush_output.

send_message(Msg) :-
	Msg = msg(_,K,_),
	retract_fact(ackleft(K, AckLeft, St)),
	NewAckLeft is AckLeft + 1,
	asserta_fact(ackleft(K, NewAckLeft, St)),
	assertz_fact(Msg),
	display(sent(Msg)),nl,
	display(ackleft(NewAckLeft)),nl.

logistics(ack(_)).
logistics(marker(_)).
logistics(prop_ended).
logistics(show_stats).
logistics(begin_labeling).
logistics(maybe_wake_up).
logistics(harvest).
logistics(enforce_backtrack). 
logistics(restart_termination_detection). 
logistics(collect(_)).
logistics(flush).

show_ranges([]).
show_ranges([V|Vs]) :-
	retrieve_range(V,R),
	display(R),nl,
	show_ranges(Vs).

show_ranges2([]).
show_ranges2([x(_,V,_)|Vs]) :-
	retrieve_range(V,R),
	display(R),nl,
	show_ranges2(Vs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Setting the initial store
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- use_module(library(goal_trans), [add_goal_trans/2]).
:- use_module(library('fd/fd_rt')).
:- use_module(library('fd/fd_tr'),[fd_tr/2]).
:- include(library(fd)).

:- use_module(library(terms_vars)).
:- use_module(library(dynamic)).

:- data vars_dic/4.
:- dynamic entails/2.
:- data t_i/2.
:- data body_part/2.
:- data '$entailments_gathered'/0.

:- multifile verify_attribute/2. 
:- multifile combine_attributes/2.

verify_attribute(Attr, Value) :-
	integer(Value),
	Attr = '$dl'(Var, VId), 
	detach_attribute(Var),
	Var = Value,
	retract_fact(vars_dic(VId, _, S, SType)),
	asserta_fact(vars_dic(VId, Var, S, SType)).

store(Vs, SiteId) :-
	clean_frame,
	do_store(Vs, SiteId, s).

do_store([],_,_) .
do_store([N|Vs], SiteId, SType) :-
	integer(N), !,
	do_store(Vs, SiteId, SType).
do_store([V|Vs], SiteId, SType) :-
	get_attribute(V, Attr), !,
	Attr = '$dl'(V, VId),
	(current_fact(vars_dic(VId,_,_,SType0)) ->
	 rearrange_store(SType, SType0, VId, V, SiteId)
	;
	 true),
	do_store(Vs, SiteId, SType).
do_store([V|Vs], SiteId, SType) :-
	create_new_id(XId),
	attach_attribute(V, '$dl'(V, XId)),
	asserta_fact(vars_dic(XId, V, SiteId, SType)),
	do_store(Vs, SiteId, SType).

rearrange_store(s, s,_,_,_) :- !.
rearrange_store(s, SType0, Va, V, SiteId) :-
	integer(SType0), !,
	retract_fact(vars_dic(Va,_,_,_)),
	asserta_fact(vars_dic(Va, V, SiteId, SType0)).
rearrange_store(_,_,_,_,_) .

	
@(C, S) :-
	integer(S),
	S >= 0,
	varset(C, L),
	do_store(L, S, S),
	asserta_fact(body_part(S, C)).

create_new_id(NewId) :-
	(current_fact(vars_dic(Id,_,_,_)) ->
	 NewId is Id + 1
	;
	 NewId = 1).

set_source :-
	current_fact(source(_)), !.
set_source :-
%	retractall(source(_)),
	findall(S, body_part(S,_), L),
	L=[S1|R],
	get_min(R, S1, Min), 
	asserta_fact(source(Min)),
	display(source(Min)), nl.

check_not_empty_store(S0) :-
	do_check_not_empty_store(S0, [], S),
	(S0 = S ->
	 true
	;
	 update_priorities(S, S0),
	 display(newsource(S0, S)), nl
	).	

do_check_not_empty_store(S, _Ids, S) :-
	current_fact(l_store(S, LL,_)),
	\+ LL = [],
	current_fact(l_store(S1,_,LR)),
	\+ S1 = S,
	intersection(LL, LR, I),
	\+ I = [], !.
do_check_not_empty_store(S0, Ids, S) :-
	findall(Id, (store(Id,_,_), S0 < Id, \+ member(Id, Ids)), LIds),
	LIds = [S1|R],
	get_min(R, S1, Min), 
	do_check_not_empty_store(Min, [S0|Ids], S).

update_priorities(S, S0) :-
 	 retract_fact(store(S0, L0, R0)),
 	 retract_fact(store(S, L, R)),
 	 retract_fact(l_store(S, LL, LR)),
 	 retract_fact(l_store(S0, LL0, LR0)),
 	 retract_fact(p_store(S, PL, PR)),
 	 retract_fact(p_store(S0, PL0, PR0)),
 	 retract_fact(t_i(S0, E0)),
 	 retract_fact(t_i(S, E)),
 	 asserta_fact(store(S0, L, R)),
 	 asserta_fact(store(S, L0, R0)),
 	 asserta_fact(l_store(S, LL0, LR0)),
 	 asserta_fact(l_store(S0, LL, LR)),
 	 asserta_fact(p_store(S, PL0, PR0)),
 	 asserta_fact(p_store(S0, PL, PR)),
	 display(new_store(S0, L, R)), nl,
 	 display(new_store(S, L0, R0)), nl,
 	 asserta(t_i(S0, E)),
 	 asserta(t_i(S, E0)).

gather_entails(VarIds) :-
	current_fact(body_part(S,_)), !,
	(current_fact(store(S,_,_)) ->
	 true
	;
	 findall(C, body_part(S, C), CList),
	 varset(CList, L0),
	 free_dups(L0, L),
	 set_store(L, VarIds, S),
	 build_entails(S, CList),
	 retractall(body_part(S,_))
	),
	gather_entails(VarIds).
gather_entails(_).

get_ids([], []) . 
get_ids([Var|Vs], [VarId|VIds]) :-
	get_attribute(Var, Attr), !,
	Attr = '$dl'(Var, VarId),
	get_ids(Vs, VIds).
get_ids([_|Vs], VIds) :-
	get_ids(Vs, VIds).

free_dups([], []).
free_dups([V|Vs], [V|FL]) :-
	get_attribute(V, Attr), !,
	Attr = '$dl'(V, Va),
	clean(Vs, Va, V, NewVs),
	free_dups(NewVs, FL).
free_dups([_V|Vs], FL) :-
 	free_dups(Vs, FL).

clean([],_,_,[]).
clean([X|Xs], Va, V, Vs) :-
	get_attribute(X, '$dl'(X, Xa)),
 	(Va = Xa ->
 	 detach_attributes([V,X]),
	 V = X,
	 attach_attribute(V, '$dl'(V, Va)),
	 Vs = VsAux
 	;
	 Vs = [X|VsAux]
	),
	clean(Xs, Va, V, VsAux).	

set_store(L, VarIds, S) :-
	do_set_store(L, S, VarIds, Locals, Remotes, LLocals, LRemotes),
	asserta_fact(store(S, Locals, Remotes)), 
	asserta_fact(p_store(S, Locals, Remotes)), 
	display(store(S, Locals, Remotes)), nl,
	display(l_store(S, LLocals, LRemotes)), nl,
	asserta_fact(l_store(S, LLocals, LRemotes)).

do_set_store([],_,_,[],[],[],[]).
do_set_store([V|Vs], S, VarIds, Locals, Remotes, LLocals, LRemotes) :-
	current_fact(vars_dic(XId,_,S,_)),
	get_attributes([V], [Va]),
	XId == Va,
	Locals = [XId|Ls], !,
	(member(XId, VarIds) ->
	 LLocals = [XId|LLs]
	;
	 LLocals = LLs),
	do_set_store(Vs, S, VarIds, Ls, Remotes, LLs, LRemotes).
do_set_store([V|Vs], S, VarIds, Locals, Remotes, LLocals, LRemotes) :-
	current_fact(vars_dic(XId,_,_,_)),
	get_attributes([V], [Va]),
	XId == Va,
	Remotes = [XId|Rs],
	(member(XId, VarIds) ->
	 LRemotes = [XId|LRs]
	;
	 LRemotes = LRs),
	do_set_store(Vs, S, VarIds, Locals, Rs, LLocals, LRs).

build_entails(Agent, Constraints) :-
	current_fact(store(Agent, Locals, Remotes)),
	varset(Constraints, L2),
	get_members(Locals, LM, L2, L),
	get_members(Remotes, RM, L2, L),
	append(LM, RM, M),
	asserta_fact(t_i(Agent, Agent)),
	asserta((entails(Agent, L) :-
		detach_attributes(L2),
		do_call(M), !,
		do_clpfd_call(Constraints))).

do_call([]).
do_call([C|Cs]) :-
	call(C),
	do_call(Cs).

do_clpfd_call([]).
do_clpfd_call([C|Cs]) :-
	clpfd_call(C),
	do_clpfd_call(Cs).
		
get_members([], [],_,_) .
get_members([VId|VIds], [M|Ms], CVars, L) :-
	current_fact(vars_dic(VId, V,_,_)),
	(integer(V) ->
	 M = (member(x(VId,V,V), L); true)
	;
	 get_equivalent(VId, V, CVars),
	 M = (member(x(VId,_,V), L); true)
	),
	get_members(VIds, Ms, CVars, L).

get_equivalent(VId, X, L) :-
	member(V, L),
	get_attribute(V, '$dl'(V, A)),
	VId = A,
	detach_attributes([X, V]),
	X = V,
	attach_attribute(X, '$dl'(X, A)).

get_attributes([], []).
get_attributes([V|Vs], [A|As]) :-
	get_attribute(V, '$dl'(V, A)), !,
	get_attributes(Vs, As).
get_attributes([I|Vs], [I|As]) :-
	integer(I), !,
	get_attributes(Vs, As).
get_attributes([_V|Vs], As) :-
	get_attributes(Vs, As).

detach_attributes([]).
detach_attributes([V|Vs]) :-
	get_attribute(V,_A), !,
	detach_attribute(V),
	detach_attributes(Vs).
detach_attributes([_V|Vs]) :-
	detach_attributes(Vs).
	
clpfd_call(Query) :-
        this_module(ThisModule),
        add_goal_trans(ThisModule, fd_tr/2),
        call(Query).

get_min([], Min, Min).
get_min([X|Xs], Min0, Min) :-
	(X < Min0 ->
	 NewMin0 = X
	;
	 NewMin0 = Min0),
	get_min(Xs, NewMin0, Min).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Harvest of results
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
collect_results(N) :-
	source(S),
	a(N, L),
	send_message(msg(S, N, collect(L))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Stats stuff
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

store_stats(Message, N, K) :-
	functor(Message, Msg_Id,_),
	phase(N, Phase),
	asserta_fact(stats(N, K, Msg_Id, Phase)).

show_stats(N) :-
	store(NId,_,_),
	send_message(msg(NId, N, show_stats)),
	fail.
show_stats(_).

dump_stats(N) :-
	nl, display('***************************************************'), nl,
	display('STATISTICS OF AGENT '), display(N), nl,
% Messages received during propagation:
	display('*****************OK (propagation)**********************'), nl,
	dump_msg(N, 'ok?', prop, N_ok_prop),
	display('ok? messages received during propagation: '),
	display(N_ok_prop), nl, nl,
	display('******************ACK (propagation)********************'), nl,
	dump_msg(N, ack, prop, N_ack_prop),
	display('ack messages received during propagation: '),
	display(N_ack_prop), nl, nl,
	display('****************NO GOOD (propagation)******************'), nl,
	dump_msg(N, no_good, prop, N_no_good_prop),
	display('no_good messages received during propagation (none, I hope): '),
	display(N_no_good_prop), nl, nl,
	display('************NEGLECTED NO GOOD (propagation)************'), nl,
	dump_msg(N, neglected_no_good, prop, N_no_no_good_prop),
	display('no_good messages neglected during propagation (none, I hope): '),
	display(N_no_no_good_prop), nl, nl,
	display('************BEGIN LABELING (propagation)***************'), nl,
	dump_msg(N, begin_labeling, prop, N_bl_prop),
	display('begin_labeling messages received during propagation (one, I hope): '), 
	display(N_bl_prop), nl, nl,
	display('***********PROPAGATION ENDED (propagation)*************'), nl,
	dump_msg(N, prop_ended, prop, N_pe_prop),
	display('End of propagation messages received: '),
	display(N_pe_prop), nl, nl,
	display('**********TERMINATION DETECTION (propagation)**********'), nl,
	dump_msg(N, marker, prop, N_marker_prop),
	display('Termination detection messages (markers) received during propagation: '),
	display(N_marker_prop), nl, nl,
% Messages received during labeling:
	display('********************OK (labeling)**********************'), nl,
	dump_msg(N, 'ok?', lab, N_ok_lab),
	display('ok? messages received during labeling: '), 
	display(N_ok_lab), nl, nl,
	display('*******************ACK (labeling)**********************'), nl,
	dump_msg(N, ack, lab, N_ack_lab),
	display('ack messages received during labeling: '),
	display(N_ack_lab), nl, nl,
	display('******************NO GOOD (labeling)*******************'), nl,
	dump_msg(N, no_good, lab, N_no_good_lab),
	display('no_good messages received during labeling: '), 
	display(N_no_good_lab), nl, nl,
	display('************NEGLECTED NO GOOD (labeling)************'), nl,
	dump_msg(N, neglected_no_good, lab, N_no_no_good_lab),
	display('no_good messages neglected during labeling: '),
	display(N_no_no_good_lab), nl, nl,
	display('**************BEGIN LABELING (labeling)****************'), nl,
	dump_msg(N, begin_labeling, lab, N_bl_lab),
	display('begin_labeling messages received during labeling (none, I hope): '), 
	display(N_bl_lab), nl, nl,
	display('*************PROPAGATION ENDED (labeling)**************'), nl,
	dump_msg(N, prop_ended, lab, N_pe_lab),
	display('End of propagation messages received during labeling (none, I hope): '),
	display(N_pe_lab), nl, nl,
	display('************TERMINATION DETECTION (labeling)***********'), nl,
	dump_msg(N, marker, lab, N_marker_lab),
	display('Termination detection messages (markers) received during labeling: '), 
	display(N_marker_lab), nl, nl.

dump_msg(Id, Msg_Id, Phase, N) :-
	retract_fact(stats(Id, K, Msg_Id, Phase)), !,
	display(Id),
	display(': '),
	display(Msg_Id),
	display(' from '),
	display(K), nl,
	dump_msg(Id, Msg_Id, Phase, N1),
	N is N1 + 1. 
dump_msg(_,_,_,0) .

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

flush_structs(N) :-
	retractall(v(N,_)),
	retractall(no_good(N,_,_)),
	retractall(a(N,_)),
	retractall(pending_ok(N,_)),
	retractall(tmp_a(N,_)),
	retractall(local_store(N,_)),
	retractall(old_assignment(N,_)),
	retractall(parent(N,_)),
	retractall(ackleft(N,_,_)),
	retractall(phase(N,_)), 
	(source(N) ->  
	  retractall(marker(_)),
	  retractall(bkup_marker(_)),
	  retractall(waiting_for_collection(_)),
	  retractall(b(_,_)), %
	  retractall(marker_counter(N,_))
 	;
 	 true),
	retractall(waiting_for_completeness(N,_)),
	retractall(waiting_for_labs(N,_)),
	retractall(stats(N,_,_,_)).
