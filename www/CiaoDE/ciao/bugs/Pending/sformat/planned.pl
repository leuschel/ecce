%:- module(planned,[planned/8,intersection/6,length/3],[assertions,basicmodes]).
:- module(planned,[planned/8,intersection/6,length/3],[assertions]).

:- use_module(library(aggregates)).

:- use_module(workplan).
:- use_module(intended_effort).

:- comment(author,"Germ@'{a}n Puebla").  

:- comment(module, "The predicate @pred{planned/8} can be queried in
   several ways in order to get different kinds of information about
   the planned effort which the different partners should devote to
   the project according to the workplan.").

:- comment(title, "Computing expected effort in a project"). 
:- comment(subtitle, "According to a set of task descriptions"). 

:- comment(summary, "This program can be used to check that the work
   distribution within the project is acceptable and to forsee the
   effort which the different partners should allocate to the
   project. The main assumption in this program is that the effort is
   evenly distributed along the life of a task, which may not always
   be the case. ").

active_tasks(Project,Start,End,Tasks):-
	findall(t(WP,T,S,E,Name),task(Project,WP,T,S,E,Name),All_Tasks),
	select_active(All_Tasks,Start,End,Tasks).

select_active([],_,_,[]).
select_active([t(WP,T,S,E,Name)|Tasks],Start,End,Active_Tasks):-
	(is_active(S,E,Start,End) ->
	    Active_Tasks = [t(WP,T,S,E,Name)|More_Tasks]
	;
	    Active_Tasks = More_Tasks),
	select_active(Tasks,Start,End,More_Tasks).

:- comment(planned(Project,Start,End,Partner,Unit,Tasks,Efforts,Total),
   "Returns in @var{Tasks} the list of tasks in @var{Project} which
   are active in the period from the beginning of month @var{Start} to
   the beginning (but not including) month @var{end} in which
   @var{Partner} participates. The value of @var{Unit} controls
   whether the effort is computed in man months (if
   @var{Unit}=@tt{mm}) or in hours (in any other case).  @var{Efforts}
   is the list of effort for each task. @var{Total} is the sum of
   efforts for all tasks.

   For example, the query @tt{planned(asap,0,12,1,mm,_,_,T).} returns
   in @tt{T} the number of MMs which partner 1 should devote to asap
   during the first year.

   The query @tt{planned(asap,0,1,3,hours,Tasks,Efforts,T).} in
   addition to returning in @var{T} the number of @em{hours} which
   partner 3 should devote to asap during the first month of the
   project (which starts in month 0 and ends in the first day of month
   1), returns in @var{Tasks} the list of tasks in which partner 3 has
   to participate and in the list @var{Efforts} the number of hours
   which should be devoted to each of those tasks.").

%:- pred planned(+,+,+,+,+,-,-,-).

% :- trust pred planned(Project,Start,End,Partner,Unit,Tasks,Efforts,Total)  
% 	=> ground([Project,Start,End,Partner,Unit]).

:- check pred planned(Project,Start,End,Partner,Unit,Tasks,Efforts,Total)  :
	ground([Project,Start,End,Partner])
 	=> ground([Tasks,Efforts,Total]).


planned(Project,Start,End,Partner,Unit,Tasks,Efforts,Total):-
	active_tasks(Project,Start,End,A_Tasks),
	get_intended_effort(A_Tasks,Project,Start,End,Partner,Tasks,Efforts_MM),
	sum_list(Efforts_MM,Total_MM),
	(Unit = mm ->
	    Efforts = Efforts_MM,
	    Total = Total_MM
	;
	    hours_in_a_MM(Partner,Num_Hours),
	    map_multiply(Efforts_MM,Num_Hours,Efforts),
	    Total is Total_MM * Num_Hours).

is_active(_S,E,Start,_End):-
	E =< Start, !, fail.
is_active(S,_E,_Start,End):-
	S >= End, !, fail.
is_active(_S,_E,_Start,_End).

get_intended_effort([],_Project,_Start,_End,_Partner,[],[]).
get_intended_effort([Task|Tasks],Project,Start,End,Partner,Active_Tasks,Intended_Effort):-
	Task = t(WP,T,S,E,_Name),
	intended_effort(Project,WP,T,Partner,Effort),
	Effort > 0, !,
	intersection(S,E,Start,End,Lower,Upper),
	length(Lower,Upper,Actual_Length),
	length(S,E,Full_Length),
	Estimated_Effort is Effort*Actual_Length/Full_Length,
	Active_Tasks = [Task|A_Tasks],
	Intended_Effort = [Estimated_Effort|I_Effort],
	get_intended_effort(Tasks,Project,Start,End,Partner,A_Tasks,I_Effort).
get_intended_effort([_Task|Tasks],Project,Start,End,Partner,Active_Tasks,Intended_Effort):-
	get_intended_effort(Tasks,Project,Start,End,Partner,Active_Tasks,Intended_Effort).

:- check pred intersection(S,E,Start,End,Lower,Upper): ground([S,E,Start,End])=>ground([Lower,Upper]).
intersection(S,E,Start,End,Lower,Upper):-
	max(S,Start,Lower),
	min(E,End,Upper).

max(A,B,C):-
	(A > B -> C = A ; C = B).
min(A,B,C):-
	(A < B -> C = A ; C = B).

:- check pred length(Start,End,Length): ground([Start,End]) => ground(Length).
length(Start,End,Length):-
	Length is End - Start.

:- comment( hours_in_a_MM(Partner,Number_of_Hours), "In the CPFs it is
fixed the number of hours a MM represents. Note that this may vary
from one site to another as it depends on the annually working hours.").

hours_in_a_MM(1,131.25). %1575/12
hours_in_a_MM(2,128.33). %1540/12
hours_in_a_MM(3,135.00). %1620/12
hours_in_a_MM(4,135.67). %1628/12


map_multiply([],_,[]).
map_multiply([X|Xs],Num,[NX|NXs]):-
	NX is X * Num,
	map_multiply(Xs,Num,NXs).

sum_list([],0).
sum_list([X|Xs],Sum):-
	sum_list(Xs,Tmp),
	Sum is Tmp + X.
