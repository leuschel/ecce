:- module(det_hook_rt,
	[det_try/3, '$pending_cut_goals'/0],
	[assertions, isomodes]).

:- use_module(engine(internals)).

:- comment(author, "Jos@'{e} Morales").
:- comment(author, "Manuel Carro").

:- comment(title, "Call on determinate").
:- comment(module, "Offers an enriched variant of call and cut
@tt{!!/0} which executes pending goals when the computation has no
more alternatives.  

This library is useful to, for example, get rid of external
connections once the necessary data has been obtained.").

:- comment(appendix, "As an example, the program

@begin{verbatim}
:- module(_, _, [det_hook]).

enumerate(X):-
        display(enumerating), nl,
        OnCut = (display('goal cut'), nl),
        OnFail = (display('goal failed'), nl),
        det_try(enum(X), OnCut, OnFail).

enum(1).
enum(2).
enum(3).
@end{verbatim}

behaves as follows:

@begin{verbatim}
?- enumerate(X).
enumerating

X = 1 ? ;

X = 2 ? ;

X = 3 ? ;
goal failed
@end{verbatim}

(note the message inserted on failure).  The execution can be cut as follows:

@begin{verbatim}
?- use_package(det_hook).
@{Including /home/clip/lib/ciao/ciao-1.7/library/det_hook/det_hook.pl
@}

yes
?- enumerate(X), '!!'.
enumerating
goal cut

X = 1 ? ;

no
@end{verbatim}").

:- comment(bug, "If the started goals do not exhaust their solutions,
and '!!'/0 is not used, the database will populate with facts which
will be consulted the next time a '!!'/0 is used.  This could cause
incorrect executions.").

:- comment(usage, "@begin{verbatim}
:- use_module(library(det_hook_rt)).
@end{verbatim}
in which case, @tt{!!/0} is not available.

Typically, this library is used as a package:
@begin{verbatim}
:- use_package(det_hook).
@end{verbatim}").

:- data cut_goal/2.

:- pred det_try(Goal, OnCut, OnFail) : callable * callable *
callable # "@var{Action} is called, and @var{OnCut} and @var{OnFail}
are goals to be executed when @var{Goal} is cut or when it finitely
fails, respectively.  In order for this to work, cutting must be
performed in a special way, by using the @pred{!!/0} predicate, also
provided by this module.".

:- meta_predicate det_try(goal, goal, goal).

det_try(Action, OnCutGoal, OnFailGoal) :-
	cut_goal(OnCutGoal),
	(Action ; remove_cut_goal, OnFailGoal, fail).
	
cut_goal(Goal) :-
	'$metachoice'(C),
	asserta_fact(cut_goal(C, Goal)).

remove_cut_goal :-
	'$metachoice'(C),
	retract_fact(cut_goal(C, _)).

:- comment(hide, '$pending_cut_goals'/0).

'$pending_cut_goals' :-
	'$metachoice'(X),
	pending_cut_goals_2(X).

pending_cut_goals_2(X) :-
	current_fact(cut_goal(C, Goal)), 
	C >= X, !,
	retract_fact(cut_goal(C, Goal)),
	Goal,
	pending_cut_goals_2(X).
pending_cut_goals_2(_).


:- comment(doinclude, '!!'/0).
:- pred '!!' # "Performs a special cut which prunes alternatives away,
as the usual cut, but which also executes the goals specified as
@var{OnCut} for any call in the scope of the cut.".
