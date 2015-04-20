:- module(platforms,_,[]).

:- initialization(init).

:- use_package(library(objects)).

:- use_class(library('agents/agents_class')).

:- use_module(library(concurrency)).
:- use_module(library(compiler)).
:- use_module(library(read),[read/1]).
:- use_module(library(system),[pause/1,time/1]).

:- use_package(argnames).

:- argnames ams_names(name,object,class,queue,alarms,goal_id,address).
:- argnames df_services(name,service,list).

:- data ams_names/7.
:- data df_services/3.
:- multifile library_directory/1.
:- dynamic library_directory/1.


platform_name(Platform) :-
    this_module(Platform).

init :-
    platform_name(Platform),
    launch_ams,
    check_factams_names${}),
    launch_df,
    check_factams_names${}),
    display(Platform),nl,!.

launch_ams:-
%   agent_id(ams),
    concurrent(AmsQueue/3),
    concurrent(AlarmQueue/2),
    eng_goal_id(Goal_id),
    platform_name(Platform),
    asserta_fact(ams_names${name => @(ams,Platform), class => ams,
    queue => AmsQueue, alarms => AlarmQueue, goal_id => Goal_id,
    address => local}),
    register_service(ams,naming).

launch_df:-
%   agent_id(df),
    concurrent(DfQueue/3),
    concurrent(AlarmQueue/2),
    eng_goal_id(Goal_id),
    platform_name(Platform),
    asserta_fact(ams_names${name => @(df,Platform),class => df,
    queue => DfQueue, alarms => AlarmQueue, goal_id => Goal_id,
    address => local}),
    register_service(df,services).

launch_agent(Name,Class):-
    ( use_class(library(Class)) ->
      eng_call(startagent(Name,Class),create,create)
    ;
      display('Error clase no encontrada')).


startagent(Name,Class):-
    platform_name(Platform),
    Agent_Name = @(Name,Platform),
    ( current_fact_nb(ams_names${name => Agent_Name}) ->
      display('Nombre ya registrado'),
      fail
    ;
     Cons =.. [Class,Agent_Name],
     Agent_Id new Cons,
     display('Agent built'(Cons)),nl,
     ( Agent_Id:main ->
       true
     ;
       display('Error en el main de' + Agent_Name),nl
     )
    ).

agent_id(Name,Id):-
    current_fact_nb(ams_names${name => @(Name,_P), object => Id}).

send(Receiver,Msg,Sender):-
    ( Receiver = @(Name,Platform) ->
      Receiver_aux = Receiver
    ;
      Receiver_aux = @(Name,Platform)
    ),
    ams_names${name => Receiver_aux, queue => Queue},
    Term =.. [Queue,Receiver_aux,Msg,Sender],
    assertz_fact(Term).

receive(Sender,Msg,Receiver):-
    ams_names${name => Receiver,queue => Queue},
    Term =.. [Queue,Receiver,Msg,Sender],
    retract_fact(Term).

send_alarm(Receiver,Alarm,Sender):-
    ( Receiver = @(Name,Platform) ->
      Receiver_aux = Receiver
    ;
      Receiver_aux = @(Name,Platform)
    ),
    ams_names${name => Receiver_aux, alarms => QAlarms},
    Term =.. [QAlarms,Alarm,Sender],
    assertz_fact(Term).

receive_alarm(Sender,Alarm,Receiver):-
    ams_names${name => Receiver, alarms => QAlarms},
    Term =.. [QAlarms,Alarm,Sender],
    display(before_alarm(Term)),nl,
    retract_fact(Term),
    display(after_alarm(Term)),nl.


check_q(X,Q):-
    ams_names${name=>X,queue => Queue},
    Term =.. [Queue,X,Msg,Sender],
    check_factTerm),
    Q = [Msg,Sender].

check_a(X,A):-
    ams_names${name => X, alarms => Alarms},
    Term =.. [Alarms,Alarm,Sender],
    check_factTerm),
    A = [Alarm,Sender].

check_agent(Agent,X):-
    ams_names${name => Agent, object => Agent_Id},
    Y = (Agent_Id:check_factX)),
    call(Y).

check_factX):-
    current_fact_nb(X).

register(Agent_Name,Object_Id,Class,Address):-
    ( current_fact_nb(ams_names${name => Agent_Name}) ->
      display('Nombre ya registrado'),nl,
      fail
    ;
     ( Address = local ->
       concurrent(AgentQueue/3),
       concurrent(AgentAlarms/2),
       eng_goal_id(Goal_Id),
       asserta_fact(ams_names${name => Agent_Name, object => Object_Id, class => Class,
       queue => AgentQueue, alarms => AgentAlarms, goal_id => Goal_Id, address => Address}),
       display('Registered' + Agent_Name),nl
     ;
       display('This Address not implemented yet' + Address),nl
     )
    ).

register_service(Name,Service):-
    asserta_fact(df_services(Service,Name,_L)).

look_for_service(Name,[Service]):-
    retract_fact(df_services(Service,Name,L)),
    assertz_fact(df_services(Service,Name,L)).

ouput(Name,P):-
    display(' << '),display(Name),display(' >> '),display(P),nl.

input(Name,Q,I):-
    display(' << '),display(Name),display(' >> '),
    display(' ¿ '),display(Q),display(' ? '),
    read(I).

wait(Seconds):-
    pause(Seconds).
