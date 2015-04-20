:- module(agents_tr, [translate/3,argnames_use/3], [assertions]).

:- use_module(library('compiler/c_itf'),[defines_module/2]).
:- use_module(library(terms), [arg/2]).

:- data argnames/4.
:- data messages/2.
:- data alarms/2.

translate((:- init_trans),[ (Cons :- agents_class(Arg,M))],M):-
	Cons =.. [M,Arg].

translate((:- message_def(Args)),[], M) :-
	( Args = [type|_Rest] -> 
	  R =.. [message|Args],
	  functor(R, F, N),
	  ( argnames(F, _, R0, M)  ->
            ( R0 == R -> true
            ; inform_user(['ERROR: incompatible message_def declaration ',R])
            )
	  ; 
	    ( arg(R, A), \+ atomic(A) ->            
	      inform_user(['ERROR: invalid message declaration ',R])
	    ; 
	      assertz_fact(argnames(F,N,R,M))
	    )
	  )
	;
	  inform_user(['ERROR: bad definition of message [type,...]', Args])
	).

translate((:- message(F)), [(message_defined(F/A))], M) :-
        ( argnames(message, N, R0, M)  ->
	  A is N-1,
	  R0 =.. [message,type|Args],
	  R =.. [F|Args],
	  assertz_fact(argnames(F,A,R,M)),assertz_fact(messages(F,A))
        ;           
	    inform_user(['ERROR: not definition for messages ',R])
        ).

translate((:- alarm_def(Args)),[], M) :-
	( Args = [type|_Rest] -> 
	  R =.. [alarm|Args],
	  functor(R, F, N),
	  ( argnames(F, _, R0, M)  ->
            ( R0 == R -> true
            ; inform_user(['ERROR: incompatible alarm_def declaration ',R])
            )
	  ; 
	    ( arg(R, A), \+ atomic(A) ->            
	      inform_user(['ERROR: invalid alarm declaration ',R])
	    ; 
	      assertz_fact(argnames(F,N,R,M))
	    )
	  )
	;
	  inform_user(['ERROR: bad definition of alarm [type,...]', Args])
	).

translate((:- alarm(F)), [(alarm_defined(F/A))], M) :-
	( argnames(alarm, N, R0, M) ->
          A is N-1,
	  R0 =.. [alarm,type|Args],
	  R =.. [F|Args],
	  assertz_fact(argnames(F,A,R,M)),assertz_fact(alarms(F,A))
	;
	  inform_user(['ERROR: there is not definition for alarms ',R])
	).

translate((:- use_messages(Name)),[(:- include(Name))],_).

translate((:- use_protocol(library(Prot),Role)), [(:- use_module(library(Prot))),  
	 (:- implements(library(Prot_Role)))],_M):-
        defines_module(Prot,Mod),
	atom_concat(Prot,'/',X),
	atom_concat(X,Mod,Y),
	atom_concat(Y,'_',Z),
	atom_concat(Z,Role,Prot_Role).

translate((:- activities(L)),List_of_activities,_M):-
	activities(L,List_of_activities).

translate((:- tasks(L)),List_of_tasks,_M):-
	tasks(L,List_of_tasks).

translate((do(before(Task),Body)),[(def_before_task(Task,Before_Task)),
	(Before_Task :- Body)],_) :-
	Task =.. [Name|Args],
	atom_concat('$bef_',Name,BName),
	Before_Task =.. [BName|Args].

translate((do(after(Task),Body)),[(def_after_task(Task,After_Task)),
	(After_Task :- Body)],_) :-
	Task =.. [Name|Args],
	atom_concat('$aft_',Name,AName),
	After_Task =.. [AName|Args].

translate((do(when($(Messg,TheArgs)),Body)), [(message_implemented(New_Messg)),(New_Messg :- Body)],M):- !,
	argnames_use($(Messg,TheArgs),New_Messg, M),
	functor(New_Messg,Name,A),
	messages(Name,A).

translate((do(when(Messg),Body)),[(message_implemented(Messg)),(Messg :- Body)],_M):-
	functor(Messg,Name,A),
	messages(Name,A).

translate((do(on($(Messg,TheArgs)), Body)), [(alarm_implemented(New_Messg)),(New_Messg :- Body)],M):- !,
	argnames_use($(Messg,TheArgs),New_Messg, M),
	functor(New_Messg,Name,A),
	alarms(Name,A).

translate((do(on(Messg),Body)),	[(alarm_implemented(Messg)),(Messg :- Body)],_M):-
	functor(Messg,Name,A),
	alarms(Name,A).

translate(C,C,_M).


argnames_use($(F,TheArgs), T, M) :-
        atom(F),
        argnames_args(TheArgs, Args),
        argnames_trans(F, Args, M, T).

argnames_trans(F, Args, M, T) :-
        argnames(F, A, R, M),
        functor(T, F, A),
        insert_args(Args, R, A, T), !.
argnames_trans(F, Args, _, _) :-
        argnames_args(TheArgs, Args), !,
        inform_user(['WARNING: invalid argnames ',F,' $ ',TheArgs,
                     ' - not translated']),
        fail.

insert_args([], _, _, _).
insert_args('=>'(F,A), R, N, T) :-
        insert_arg(N, F, A, R, T).
insert_args(('=>'(F,A), As), R, N, T) :-
        insert_arg(N, F, A, R, T),
        insert_args(As, R, N, T).

insert_arg(N, F, A, R, T) :-
        N > 0,
        (   arg(N, R, F) ->
                arg(N, T, A)
        ;   N1 is N-1,
            insert_arg(N1, F, A, R, T)
        ).

argnames_args({}, []).
argnames_args({Args}, Args).

activities([Act],[(activity(Act))]).

activities([Act|Rest],[(activity(Act))|Rest_Act]):-
	activities(Rest,Rest_Act).

tasks([Task],[(task(Task))]).
tasks([Task|Rest],[(task(Task))|Rest_Task]):-
	tasks(Rest,Rest_Task).
