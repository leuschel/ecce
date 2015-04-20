%%------------------------------------------------------------------------
%%
%% A'Ciao: Agent Oriented Programming in Ciao/Prolog
%%
%% SYNTAX FILE FOR Agents
%%
%% AUTHOR : Sergio Guadarrama Cotado
%% DATE   : July 2002
%%
%%------------------------------------------------------------------------
%%------------------------------------------------------------------------

:- class(agents_class).

:- use_package(argnames).
:- argnames message(type,sender,receiver,content,id).


:- use_module(library(concurrency)).
:- use_module(library(platforms)).

:- export([agent_name/1,ccheck/1]).

:- inheritable([add_task/2,end_task/1,remove_task/1,do_task/2]).
:- inheritable([continue_condition/2,continue_task/1]).
:- inheritable([send/2,receive/2,register/1,register_service/1,show/1]).

:- virtual '$before_task'/1.
:- virtual '$do_task'/1.
:- virtual '$after_task'/1.
:- virtual '$check_protocol'/3.
:- virtual '$check_message'/2.
:- virtual '$process_message'/1.
 
:- data agent_obj_id/1.
:- data agent_short_name/1.
:- data agent_full_name/1.
:- data tasks_condition/2.
:- data sended/2.
:- data received/2.


:- concurrent running_tasks/2.
:- concurrent finished_tasks/2.
:- concurrent ready_tasks/2.


agents_class(@(Name,Platform),Class) :-
    self(Obj_Id),
    set_fact(agent_obj_id(Obj_Id)),
    set_fact(agent_short_name(Name)),
    set_fact(agent_full_name(@(Name,Platform))),
    eng_goal_id(Id),
    register(@(Name,Platform),Obj_Id,Class,local),
    show([Obj_Id,Id]),
    start(taskmanager),
    start(finishmanager),
    start(alarm_manager),
    eng_status.


agent_name(Name):-
    var(Name),
    current_fact(agent_full_name(Name)).

ccheck(P):-
    current_fact_nb(P).


add_task(Task,Condition):-
    show(task_added(Task)),
    asserta_fact(tasks_condition(Task,Condition)),
    asserta_fact(ready_tasks(Task,_)).

remove_task(Task) :-
    ( current_fact_nb(running_tasks(Task,Id1)) ->
      ( eng_goal_id(Id2), Id1 == Id2 ->
        show('Error, one task can not remove itself')
      ;
        end_task(Task),
        wait(1),
        eng_kill(Id1),
        show(task_removed(Task))
      )
    ).

end_task(Task):-
    ( retract_fact_nb(running_tasks(Task,_)),
      retract_fact_nb(tasks_condition(Task,_)) ->
      show(ended(Task))
    ;
      true
    ).

do_task(Task,Condition):-
    show(do_task(Task)),
    asserta_fact(tasks_condition(Task,Condition)),
    eng_goal_id(Id),
    asserta_fact(running_tasks(Task,Id)),
    display('Before' + Task + Id),nl,
    '$before_task'(Task),
    display('Starting' + Task + Id),nl,
    '$do_task'(Task),
    end_task(Task),
    '$after_task'(Task),
    display('After' + Task + Id),nl,
    show(['Task Finished', Task, Id]).

continue_task(Task) :-
    ( current_fact_nb(running_tasks(Task,_)),
      current_fact_nb(tasks_condition(Task,Condition)),
      call(Condition) ->
       true
    ;
       fail).

continue_condition(Task,Condition):-
    retract_fact(tasks_condition(Task,_)),
    asserta_fact(tasks_condition(Task,Condition)).

send(Receiver,Mesg):-
    agent_full_name(Sender),
    ( Mesg =.. [message,_Type,Sender,Receiver|_Arg] ->
      platforms:send(Receiver,Mesg,Sender),
      show(sended(Mesg))
    ;
      ( Mesg =.. [alarm,_Type,Sender,Receiver|_Arg] ->
        platforms:send_alarm(Receiver,Mesg,Sender),
        show(sended(Mesg))
      ;
        show(['Error trying to send a bad message o alarm: ',Mesg])
      )
    ).

receive(Sender,Mesg):-
    agent_full_name(Receiver),
    platforms:receive(Sender,Mesg,Receiver),
    show(recived(Mesg)),
    Mesg =.. [message,Type|Arg],
    Term =.. [Type|Arg],
    ( '$check_message'(Term,implemented) ->
      ( '$process_message'(Term) ->
        show(processed(Term))
      ;
        show(['Error processing',Term])
      )
    ;
      show(['Error: mensage recibido y no implementado',Mesg])
    ).

register(_Platform). %% Servirá para registrase en otras plataformas

register_service([Service]):-
    agent_full_name(Name),
    platforms:register_service(Name,Service),
    show('Service registered'(Service)).

show(P):-
    agent_full_name(Name),
    ouput(Name,P).

%send(Receiver,Mesg):-
%   Mesg = message(Per,Sen,Rec,_Rep_to,_Cont,Lan,_Enc,_Ont,Prot,Conv,_Rep_wi,_In_rep,_Rep_by),
%        ground(Per), agent_name(X), X = Sen, Receiver = Rec,
%   ( var(Lan) -> Lan = prolog
%   ),
%   ( var(Conv) ->
%     new_conversation(Conv),
%     ( ground(Prot) ->
%       check_protocol(Prot,Per,init)
%     )
%   ;
%     ( current_fact(received(Conv,RMesg)) ->
%       ( ground(Prot) ->
%         check_protocol(Prot,Mesg,RMesg)
%       )
%     ;
%       show('Error conversación no iniciada'),!,fail
%     )
%   ),
%   platforms:send(Receiver,Mesg,X),
%   asserta_fact(sended(Conv,Mesg)).

%receive(Sender,Mesg):-
%   agent_name(Receiver),
%   platforms:recive(Sender,Mesg,Receiver),
%   Mesg = message(Per,Sender,Receiver,_Rep_to,Content,_Lan,_Enc,
%   _Ont,_Prot,Conv,_Rep_wi,_In_rep,_Rep_by),
%   asserta_fact(received(Conv,Mesg)),
%   Term =.. [Per,Sender,Content,Conv],
%   self(X), Y = (X:Term), call(Y).


start(Manager) :-
    asserta_fact(tasks_condition(Manager,true)),
    self(S), Y = (S:Manager),show(Y),
    eng_call(Y,create,create,Id),
    asserta_fact(running_tasks(Manager,Id)),
    show([start,Manager,Id]).

alarm_manager :-
    agent_full_name(Name),
    platforms:receive_alarm(_Sender,Alarm,Name),
    show(recived_alarm(Alarm)),
    Alarm =.. [alarm,Type|Arg],
    Term =.. [Type|Arg],
    ( self(S), Y = (S:Term), call(Y) ->
      show(processed(Term))
    ;
      show('Error processing'(Term))
    ),
    alarm_manager.

taskmanager:-
    retract_fact(ready_tasks(X,C)),
    show(['Task Ready',X,C]),
    eng_call(start_agent_task(X),create,create,_Id),
    taskmanager.

start_agent_task(Task):-
    eng_goal_id(Id),
    asserta_fact(running_tasks(Task,Id)),
    display('Before' + Task + Id),nl,
    '$before_task'(Task),
    display('Starting' + Task + Id),nl,
    '$do_task'(Task),
    end_task(Task),
    '$after_task'(Task),
    display('After' + Task + Id),nl,
    asserta_fact(finished_tasks(Task,Id)).

finishmanager:-
    retract_fact(finished_tasks(T,Id)),
    show(['Finished Task',T,Id]),
    eng_wait(Id),
    eng_release(Id),
    show(['Task deleted',T,Id]),
    eng_status,
    finishmanager.


destructor:-
    display('Terminado'),
    retractall_fact(tasks_condition(_,_)),
    retractall_fact(ready_tasks(_,_)),
    retractall_fact(running_tasks(_,_)),
    retractall_fact(finished_tasks(_,_)).


%%----------------------------------------------------------------
%% To include in all agent class.
%%----------------------------------------------------------------

:- data def_before_task/2.
:- data def_after_task/2.

:- data message_defined/1.
:- data message_implemented/1.
:- data alarm_defined/1.
:- data alarm_implemented/1.

'$before_task'(Task):-
    ( def_before_task(Task,Before),
      self(S), Y = (S:Before), call(Y) ->
      true
    ;
      true
    ).


'$do_task'(Task):-
    show(' do_taks de agent'),
    ( self(S), Y = (S:Task), call(Y) ->
      ( continue_task(Task) ->
        display('Redo' + Y),nl,
        '$do_task'(Task)
      ;
        true
      )
    ;
      show(['Error doing task',Task])
    ).

'$after_task'(Task):-
    ( def_after_task(Task,After),
      self(S), Y = (S:After), call(Y) ->
      true
    ;
      true
    ).

'$check_protocol'(_,_,_).

'$check_message'(Messg,implemented):-
    message_implemented(Messg),!.

'$check_message'(Messg,defined) :-
    functor(Messg,M,A),
    message_defined('/'(M,A)),!.

'$check_message'(_,not_defined).

'$process_message'(Term):-
 (self(X), Y = (X:Term), call(Y)).

%%--------------------------------------------------------------
%% Sólo para pruebas borrar.
%%--------------------------------------------------------------

%:- export(main/0).

main:-  eng_goal_id(Id),
    show(doing(main,Id)),
    add_task(mio(pepe),fail),
    wait(3),
    add_task(wakeup,fail),
    show(ended(main,Id)).

wakeup :-
    eng_goal_id(Id),
    show(doing(wakeup(Id))),
    wait(5),
    do_task(mio(luis),fail),
    end_task(wakeup).

mio(X):-
    eng_goal_id(Id),
    show(doing(mio(X),Id)),
    end_task(mio(X)).


%:- comment(version_maintenance,on).


%% Note that the "assertions" library needs to be included in order
%% to support ":- comment(...,...)." declarations such as these.
%% These version comment(s) can be moved elsewhere in the file.
%% Subsequent version comments will be placed above the last one
%% inserted.

%:- comment(version(0*1+0,2002/09/03,10:59*17+'Hora estándar romance'),"adsf ()").
