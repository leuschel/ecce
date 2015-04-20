:- module(plat,_,[]).

:- use_package(library(objects)).
:- use_module(library(concurrency)).
:- use_module(library(read),[read/1]).
:- use_module(library(system),[pause/1,time/1]).

:- data ams_names/5.
:- data df_services/3.
%:- multifile library_directory/1.
%:- dynamic library_directory/1.

%library_directory('c:/ciao-1.8p2Win32/library/agents/examples').

init :-
	this_module(Module),
	display(Module),nl,
	launch_ams,
	check(ams_names(X,Y,Z,R,T)),
	display(ams_names(X,Y,Z,R,T)),nl,
	launch_df,
	check(ams_names(X1,Y1,Z1,R1,T1)),
	display(ams_names(X1,Y1,Z1,R1,T1)),nl.

launch_ams:-
%	agent_id(ams),
	concurrent(AmsQueue/3),
	eng_goal_id(Goal_id),
	register(ams,_,ams,AmsQueue,Goal_id),
	register_service(ams,[naming]).
	
launch_df:-
%	agent_id(df),
	concurrent(DfQueue/3),
	eng_goal_id(Goal_id),
	register(df,_,df,DfQueue,Goal_id),
	register_service(df,[services]).
	
launch_agent(Agent,Module):-
	( use_class(library(Module)) ->
%	  startagent(Agent,Module)
	  eng_call(startagent(Agent,Module),create,create)
	;
	  display('Error clase no encontrada')).

	
startagent(Agent_Name,Module):-
	( current_fact_nb(ams_names(Agent_Name,_,_,_,_)) ->
	  display('Nombre ya registrado'),
	  fail
	;
	 Cons =.. [Module,Agent_Name],
	 Agent_Id new Cons,
	 concurrent(AgentQueue/3),
	 eng_goal_id(Goal_Id),
	 register(Agent_Name,Agent_Id,Module,AgentQueue,Goal_Id),
	 Agent_Id:main).

check(X):-
	current_fact_nb(X).

register(Name,Obj_id,Module,Queue,Goal_id):-
	( current_fact_nb(ams_names(Name,_,_,_,_)) ->
	  display('Nombre ya registrado'),
	  fail
	;
	  asserta_fact(ams_names(Name,Obj_id,Module,Queue,Goal_id))).

register_service(Name,[Service]):-
	asserta_fact(df_services(Service,Name,_L)).

look_for_service(Name,[Service]):-
	retract_fact(df_services(Service,Name,L)),
	assertz_fact(df_services(Service,Name,L)).

ouput(Name,P):- 
	display(Name),display('<<'),display(P),display('>>'),nl.

input(Name,Q,I):-
	display(Name),display(' ¿ '),display(Q),display(' ? '),
	read(I).

wait(Seconds):-
	pause(Seconds).
