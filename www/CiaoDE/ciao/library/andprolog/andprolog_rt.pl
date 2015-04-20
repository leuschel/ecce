:- module(
        andprolog_rt,
        [
	    '&'/2,
	    '&!'/2,
	    '&>'/2,
	    '&>!'/2,
	    '<&'/1,
	    '<&!'/1,
	    '&'/1,
	    '&!'/1,
	    '&&'/2,
	    '&&!'/2,
	    '&&'/1,
	    '&&!'/1,
	    '&&>'/2,
	    '&&>!'/2,
	    '<&&'/1,
	    '<&&!'/1,
	    set_max_number_agents/1,
	    get_max_number_agents/1,
	    create_agents/1,
	    indep/1,
	    indep/2
	],
	[assertions, isomodes]
	 ).

:- include(andprolog_ops).

:- set_prolog_flag(multi_arity_warnings, off).

:- use_module(library(concurrency)).
:- use_module(library(conc_aggregates)).
:- use_module(library(prolog_sys)).
:- use_module(library(system)).
:- use_module(library(odd)).
:- use_module(library(lists)).

:- comment(title,  "And-parallel execution").
:- comment(author, "Amadeo Casas").
:- comment(author, "@tt{http://www.cs.unm.edu/~amadeo}").
:- comment(author, "University of New Mexico").
:- comment(usage,  "This library allows and-parallel execution of goals in
                   (Herbrand-)independent fashion. It resembles the
                   execution rules of &-Prolog.").


:- concurrent goal_stack/5.
:- concurrent goal_solution/3.

:- concurrent idle_agents/1.
:- concurrent number_agents/1.
:- concurrent max_number_agents/1.


%%***************************************************************************

:- comment(bug, "Cleaning the WAMs in pending solutions when the two goals
   have failed is not done yet.").

:- pred clean(+task_id).

:- comment(clean(TaskId), "Cleans the chunks of memory used by the goals
   that have been executed in @var{TaskId}.").

clean(TaskId) :-
	retractall_fact(goal_stack(_, _, _, _, TaskId)),
	retractall_fact(goal_solution(_, _, TaskId)).

:- pred '&>'(+callable,-handler).

:- comment('&>'(X,H), "Sends out goal @var{X}, to be executed potentially
   by another worker of the team, returning in @var{H} a handler of the
   goal sent.").

:- meta_predicate((goal&>_)).
Goal &> goal_info(WamId, Goal, SchemeGoal, TaskId) :-
	new_agent,
	new_atom(TaskId),
	copy_term(Goal, SchemeGoal),
	assertz_fact(goal_stack(SchemeGoal, call, nondet, WamId, TaskId)),
	!,
	undo(clean(TaskId)).

:- pred '&>!'(+callable,-handler).

:- comment('&>!'(X,H), "Sends out the deterministic goal @var{X}, to be
   executed potentially by another worker of the team, returning in @var{H}
   a handler of the goal sent.").

:- meta_predicate((goal'&>!'_)).
Goal '&>!' goal_info(WamId, Goal, _, TaskId) :-
	new_agent,
	new_atom(TaskId),
	assertz_fact(goal_stack(Goal, call, det, WamId, TaskId)),
	!.

:- pred '&&>'(+callable,-handler).

:- comment('&&>'(X,H), "Fair version of the &>/2 operator. If there is no
   idle worker, create one to execute goal @var{X}. This way, fairness
   among concurrent threads is ensured.").

:- meta_predicate((goal&&>_)).
Goal &&> goal_info(WamId, Goal, SchemeGoal, TaskId) :-
	ensure_agent,
	new_atom(TaskId),
	copy_term(Goal, SchemeGoal),
	assertz_fact(goal_stack(SchemeGoal, call, nondet, WamId, TaskId)),
	!,
	undo(clean(TaskId)).

:- pred '&&>!'(+callable,-handler).

:- comment('&&!'(X,H), "Fair version of the '&>!'/2 operator. If there is
   no idle worker, create one to execute goal @var{X}.").

:- meta_predicate((goal'&&>!'_)).
Goal '&&>!' goal_info(WamId, Goal, _, TaskId) :-
	ensure_agent,
	new_atom(TaskId),
	assertz_fact(goal_stack(Goal, call, det, WamId, TaskId)),
	!.

:- pred '<&'(+handler).

:- comment('<&'(H), "Gets the result of the goal pointed to by @var{H}, or
   executes it if it has not been executed yet. Backtracking of the goal
   will be done at this point.").

goal_info(WamId, Solution, Goal, TaskId) <& :-
	goal_info(WamId, Solution, Goal, TaskId) <&& .

:- pred '<&!'(+handler).

:- comment('<&'(H), "Gets the result of the deterministic goal pointed to
   by @var{H}, or executes it if it has not been executed yet.").

goal_info(WamId, Solution, Goal, TaskId) '<&!' :-
	goal_info(WamId, Solution, Goal, TaskId) '<&&!' .

:- pred '<&&'(+handler).

:- comment('<&&(H)', "Fair version of the <&/1 operator.").

goal_info(WamId, Solution, Goal, TaskId) <&& :-
	retract_fact_nb(goal_stack(Solution, call, nondet, WamId, TaskId)),
	!,
	undo(assertz_fact(goal_stack(Goal, call, nondet, WamId, TaskId))),
	call(Solution).
goal_info(WamId, Solution, Goal, TaskId) <&& :-
	retract_fact(goal_solution(Sol, WamId, TaskId)),
	(
	    Sol = end,
	    eng_wait(WamId),
	    eng_release(WamId),
	    !,
	    assertz_fact(goal_stack(Goal, call, nondet, _, TaskId)),
	    fail
	;
	    processing_generated_solution(Sol, Solution, WamId, TaskId)
	).

:- pred '<&&!'(+handler).

:- comment('<&&!'(H), "Fair version of the '<&!'/1 operator.").

goal_info(WamId, Solution, _, TaskId) '<&&!' :-
	retract_fact_nb(goal_stack(Solution, call, det, WamId, TaskId)),
	call(Solution),
	!.
goal_info(_, Solution, _, TaskId) '<&&!' :-
	retract_fact(goal_solution(Solution, _, TaskId)),
	!.

:- pred
   processing_generated_solution(+callable,+callable,+wam_id,+task_id).

:- comment(processing_generated_solution(Solution, Goal, WamId, TaskId),
   "Uses the obtained solution @var{Solution} of the goal @var{Goal} or
   asks for a new solution of it.").

processing_generated_solution(Solution, Solution, _, _).
processing_generated_solution(_, Goal, WamId, TaskId) :-
	assertz_fact(goal_stack(Goal, backtrack, nondet, WamId, TaskId)),
	fail.

:- pred '&'(+callable,+callable).

:- comment('&'(X,Y), "Performs a parallel fork of the two goals @var{X} and
   @var{Y} involved and waits for the execution of both to finish. If no
   workers are idle then the two goals may be executed by the same worker
   and sequentially, i.e., one after the other.").

:- meta_predicate((goal&goal)).
GoalA & GoalB :-
	GoalB &> H,
	GoalA,
	H <& .

:- pred '&!'(+callable,+callable).

:- comment('&!'(X,Y), "Performs a parallel fork of the two deterministic
   goals @var{X} and @var{Y} involved and waits for the execution of both
   to finish.").

:- meta_predicate((goal'&!'goal)).
GoalA '&!' GoalB :-
	GoalB '&>!' H,
	GoalA,
	H '<&!' .

:- pred '&&'(+callable,+callable).

:- comment('&&'(X,Y), "Fair version of the '&'/2 operator.").

:- meta_predicate((goal&&goal)).
GoalA && GoalB :-
	GoalB &&> H,
	GoalA,
	H <&& .

:- pred '&&!'(+callable,+callable).

:- comment('&&!'(X,Y), "Fair version of the '&!'/2 operator.").

:- meta_predicate((goal'&&!'goal)).
GoalA '&&!' GoalB :-
	GoalB '&&>!' H,
	GoalA,
	H '<&&!' .

:- pred '&'(+callable).

:- comment('&'(X), "Sends out goal @var{X} to be executed potentially by
   another worker of the team. No waiting for its return is
   performed. Updates on the variables of @var{X} will be exported to other
   workers sharing them.").

:- meta_predicate((goal&)).
Goal & :-
	new_agent,
	assertz_fact(goal_stack(Goal, call, nondet, _, _)),
	!.

:- pred '&!'(+callable).

:- comment('&!'(X), "Sends out deterministic goal @var{X} to be executed
   potentially by another worker of the team. No waiting for its return is
   performed. Updates on the variables of @var{X} will be exported to other
   workers sharing them.").

:- meta_predicate((goal'&!')).
Goal '&!' :-
	new_agent,
	assertz_fact(goal_stack(Goal, call, det, _, _)),
	!.

:- pred '&&'(+callable).

:- comment('&&'(X), "Fair version of the '&'/2 operator.").

:- meta_predicate((goal&&)).
Goal && :-
	ensure_agent,
	assertz_fact(goal_stack(Goal, call, nondet, _, _)),
	!.

:- pred '&&!'(+callable).

:- comment('&&!'(X), "Fair version of the '&!'/2 operator.").

:- meta_predicate((goal'&&!')).
Goal '&&!' :-
	ensure_agent,
	assertz_fact(goal_stack(Goal, call, det, _, _)),
	!.


%%***************************************************************************

idle_agents(0).
number_agents(0).
max_number_agents(10).

:- pred set_max_number_agents(+int).

:- comment(set_max_number_agents(N), "Sets @var{N} as the maximum number of
   agents that can be executing the different goals.").

set_max_number_agents(N) :-
	N > 0,
	retract_fact(max_number_agents(_)),
	!,
	assertz_fact(max_number_agents(N)).

:- pred get_max_number_agents(-int).

:- comment(get_max_number_agents(N), "Returns the maximum number of agents
   that can be executing the different goals in @var{N}.").

get_max_number_agents(N) :-
	var(N),
	current_fact(max_number_agents(N)),
	!.

:- pred increase_number_agents # "Increase the number of agents in the
   system.".

increase_number_agents :-
	retract_fact(number_agents(N)),
	!,
	N1 is N + 1,
	assertz_fact(number_agents(N1)).

:- pred get_max_number_agents(-int).

:- comment(get_max_number_agents(N), "Returns the number of agents @var{N}
   in the system.").

get_number_agents(N) :-
	var(N),
	current_fact(number_agents(N)),
	!.

:- pred increase_number_agents # "Increase the number of idle agents in the
   system.".

increase_idle_agents :-
	retract_fact(idle_agents(N)),
	!,
	N1 is N + 1,
	assertz_fact(idle_agents(N1)).

:- pred decrease_number_agents # "Decrease the number of idle agents in the
   system.".

decrease_idle_agents :-
	retract_fact(idle_agents(N)),
	!,
	N1 is N - 1,
	assertz_fact(idle_agents(N1)).

:- pred get_idle_agents(-int).

:- comment(get_idle_agents(N), "Returns the number of idle agents @var{N}
   in the system.").

get_idle_agents(N) :-
	var(N),
	current_fact(idle_agents(N)),
	!.

:- pred ensure_agent # "Always creates an agent.".

ensure_agent :-
	get_idle_agents(N),
	N = 0,
        create_agent.
ensure_agent.

:- pred ensure_agent # "Creates an agent if the number of agents in the
   system did not reach a fixed maximum value.".

new_agent :-
	get_number_agents(N1),
	get_max_number_agents(N2),
	(
	    (N1 < N2) -> (create_agent)
	;
	    true
	).
new_agent.

:- pred create_agents(+int).

:- comment(create_agents(N), "Creates @var{N} new agents to execute the
   different goals.").

create_agents(0) :- !.
create_agents(N) :-
        N > 0,
        create_agent,
        N1 is N - 1,
        create_agents(N1).

create_agent :-
        eng_call(agent, create, create, _).

:- pred agent # "A thread obtains a new goal to execute (or to backtrack
   over) and then returns a solution of it. When it completes these
   operations the thread waits until it obtains a new goal to run.".

agent :-
	increase_number_agents,
	increase_idle_agents,
	retract_fact(goal_stack(Goal, Action, Det, WamId, TaskId)),
	decrease_idle_agents,
	action(Action, Det, Goal, WamId, TaskId),
	increase_idle_agents,
	fail.

:- pred action(+action,+det,+callable,+wam_id,+task_id).

:- comment(action(Action,Det,Goal,WamId,TaskId), "A thread gets a new
   solution @var{Goal} of a goal, which can be the first solution of the
   goal or a new solution after backtracking over the goal, depending on
   the values of @var{Action} and @var{Det}.").

action(call, nondet, Goal, WamId, TaskId) :-
	eng_call(wrapper(Goal, WamId, TaskId), create, self, WamId).
action(call, det, Goal, _, TaskId) :-
 	call(Goal),
 	assertz_fact(goal_solution(Goal, _, TaskId)),
	!.
action(backtrack, _, _, WamId, _) :-
	eng_backtrack(WamId, self).

:- pred wrapper(+callable,+wam_id,+task_id).

:- comment(wrapper(Goal,WamId,TaskId), "Wraps the execution of the goal and
   the assertion of the solution.").

wrapper(Goal, WamId, TaskId) :-
 	call(Goal),
 	eng_goal_id(WamId),
 	assertz_fact(goal_solution(Goal, WamId, TaskId)).
wrapper(_, WamId, TaskId) :-
	eng_goal_id(WamId),
	assertz_fact(goal_solution(end, WamId, TaskId)).


%%***************************************************************************

:- true prop indep(X,Y) + native(indep([[X,Y]]))
	# "@var{X} and @var{Y} do not have variables in common.".
 
indep(A,B) :- 
        mark(A,Ground),  % Ground is var if A ground
        nonvar(Ground),  % If 1st argument was ground, no need to proceed
        marked(B), !,
        fail.
indep(_,_).

mark('$$Mark', no) :- !.        % Mark the variable, signal variable found
mark( Atom   , _ ) :- atomic(Atom),!.
mark(Complex , GR) :- mark(Complex,1,GR).

mark(Args,Mth,GR) :-
        arg(Mth,Args,ThisArg),!,
        mark(ThisArg,GR),
        Nth is Mth+1,
        mark(Args,Nth,GR). 	
mark(_,_,_).

marked( Term )  :-
        functor(Term,F,A),
        (  A > 0, !, marked(Term,1)
        ;  F = '$$Mark' ).

marked(Args,Mth) :-
        arg(Mth,Args,ThisArg),!,
        (  marked(ThisArg)
        ;  Nth is Mth+1,
           marked(Args,Nth)
	).

:- true prop indep(X) + native(indep(X))
	# "The variables in pairs in @tt{@var{X}} are pairwise independent.".

indep([]).
indep([[X,Y]|L]):- indep(X,Y), indep(L).


%%***************************************************************************

:- prop andcallable/1.

:- comment(andcallable(Exp), "@var{Exp} is of the form @tt{goal&goal}.").

andcallable(A&B):- callable(A), callable(B).

:- prop handler/1.

:- comment(handler(H), "@var{H} is a variable which stores a handler of a
   goal sent to be executed by other worker.").

handler(H) :-
	H = goal_info(WamId, Goal, SchemeGoal, TaskId),
	wam_id(WamId),
	callable(Goal),
	callable(SchemeGoal),
	task_id(TaskId).

:- prop task_id/1.

:- comment(task_id(X), "@var{X} is the id of the task published to be
   executed concurrently.").

task_id(X) :- atom(X).

:- prop wam_id/1.

:- comment(wam_id(Id), "@var{Id} is the id of the WAM where the goal is/has
   being executed.").

wam_id(Id) :- Id = '$goal_id'(_,_).

:- prop action/1.

:- comment(wam_id(Act), "@var{Act} specifies if the goal has to be executed
   or backtracked over.").

action(Act) :- (Act = call ; Act = backtrack).

:- prop det/1.

:- comment(det(D), "@var{D} shows whether the goal to be executed is
   deterministic or not.").

det(D) :- (D = det ; D = nondet).


