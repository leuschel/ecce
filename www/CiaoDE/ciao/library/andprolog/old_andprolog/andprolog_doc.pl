:- use_package(assertions).
:- comment(nodoc,assertions).


:- comment(title,"Independent and-parallel execution").
:- comment(author,"Manuel Carro").
:- comment(author,"Manuel Hermenegildo").

:- comment(module,

"@bf{Note:} This is just a partial first shot. The real library still
needs to be written. Not difficult, just no time...

This library will eventually allow and-parallel execution of goals in
(Herbrand-)independent fashion. It resembles the execution rules of
&-Prolog @cite{iclp90-performance}. Basically, goals are run in
and-parallel @em{provided that their arguments do not share bindings},
i.e., are not bound to terms which contain a common variable.").

:- include(library('andprolog/andprolog_ops')).

:- comment(doinclude, (&)/2).
:- comment(GoalA & GoalB, "@var{GoalA} and @var{GoalB} are run in
independent and-parallel fashion.  This is just a first sketch, and
valid only for deterministic independent goals.  The use is as

@begin{verbatim}
q:- a & b.
@end{verbatim}

which would start @tt{a} and @tt{b} in separate threads (possibly in
parallalel, if the machine architecture and operating system allows
that), and continue when @bf{both} have finished.  This type of
execution is safe only when @tt{a} and @tt{b} are independent in the
sense of variable sharing.  This condition can be tested with the
@pred{indep/2} predicate.
").

:- comment(doinclude, active_agents/1).
:- comment(active_agents(NumberOfAgents), "Tests/sets the
@var{NumberOfAgents} which are active looking for goals to execute.
As for now, those agents are resource-consuming, even when they are
just looking for work, and not executing any user goals.").

%:- comment(active_agents(NumberOfAgents)

:- comment(doinclude, indep/2).
:- comment(indep(X,Y), "@var{X} and @var{Y} are @index{independent},
   i.e., they are bound to terms which have no variables in
   common. For example, @tt{indep(X,Y)} holds for @tt{X=f(Z),Y=g(K)}
   and also for @tt{X=f(a),Y=X} (since both @tt{X} and @tt{Y} are
   bound to ground terms). It does not hold for @tt{X=f(Z),Y=g(Z)} and
   for @tt{X=Y}.").

 %% :- pred indep(X,Y) 
 %% # "@var{X} and @var{Y} do not have variables in common.".

:- comment(doinclude, indep/1).  :- comment(indep(X), "@var{X} is a
list of lists of length two, i.e., a list of the form @tt{[[T1, T2],
[T3, T4], ...]}.  The variables in each pair of the list @tt{@var{X}}
are tested for independence using @pred{indep/2}.  This list-of-pairs
        format is the output of several independdnce analyzers for
 pair sharing.").

 %% :- pred indep(X) # "The variables in pairs in @tt{@var{X}} are
 %% pairwise independent.".

:-comment(bug,"@bf{Beware:} the current code is just a partial first
shot. It is provided for the sole purpose of experimentation and
development.").

:- comment(bug, "The fact that only the first solution is returned for
the conjunction is due to performance issues (and lack of time), and
we expect to remove it in a near future.").

:- comment(bug,"CGEs (i.e., @tt{=>}) are not supported.").

:- comment(bug, "The @pred{indep/1}, @pred{indep/2}, and
@pred{ground/1} tests are not very efficient; they will be replaced by
native versions (taken from the &-Prolog code) in the future.").
