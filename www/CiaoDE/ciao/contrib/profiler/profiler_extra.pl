:- module(profiler_extra,[
	 measure/2
	,measure/3
	,get_option/1
],[assertions]).

:- meta_predicate measure(goal,?).
:- meta_predicate measure(?,goal,?).

:- use_module(library(prolog_sys)).
:- comment(module,"Implementation of some predicates related with the
   profiler, but that don't requires the activation of the profiling
   option in the engine.").

:- data get_option/1.

% note that better is to use usertime, the problem is that currently
% for a better function, it requires the kernel reconfiguration :-S
% due to operating system limitations.

%get_option(usertime).
get_option(walltime).

:- true pred measure(in(Goal), go(Value)) :: term * num # "Same as
   measure/3, but uses usertime to measure the time.".

measure(Goal,Value) :-
	get_option(Option),
	measure(Option,Goal,Value).

:- true pred measure(in(Option), in(Goal), go(Value)) :: atom * term *
   num # "Unifies @var{Value} with the time spent in evaluate
   @var{Goal}, using the type of time @var{Option}.".

measure(Option,Goal,Value) :-
	statistics(Option,[T1,_]),
	\+ \+ call(Goal),
	statistics(Option,[T2,_]),
	Value is T2 - T1.
