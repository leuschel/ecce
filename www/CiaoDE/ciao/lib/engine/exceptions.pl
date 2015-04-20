:- module(exceptions, [
        catch/3, intercept/3, throw/1, halt/0, halt/1, abort/0],
        [assertions, isomodes]).

:- use_module(engine(internals), ['$exit'/1]).
:- use_module(engine(basiccontrol), ['$metachoice'/1,'$metacut'/1]).
:- use_module(engine(hiord_rt), ['$meta_call'/1]).

:- comment(title, "Exception handling").

:- comment(author, "The CLIP Group").

:- comment(usage, "These predicates are builtin in Ciao, so nothing special
   has to be done to use them.").

:- comment(module, "This module includes predicates related to
   exceptions, which alter the normal flow of Prolog.").

:- primitive_meta_predicate(catch(goal, ?, goal)).
:- primitive_meta_predicate(intercept(goal, ?, goal)).

:- true pred halt + (iso, native).

:- comment(halt, "Halt the system, exiting to the invoking shell.").

halt :- '$exit'(0).

:- true pred halt(+int) + iso.

:- comment(halt(Code), "Halt the system, exiting to the invoking shell,
   returning exit code @var{Code}.").

halt(E) :- integer(E), !, '$exit'(E).
halt(V) :- var(V), !, throw(error(instantiation_error,halt/1-1)).
halt(N) :- throw(error(type_error(integer, N),halt/1-1)).

:- comment(abort, "Abort the current execution.").

abort :- '$exit'(-32768).

%------ errors ------%

:- data catching/3, thrown/1.

:- true pred catch(+callable,?term,+callable) + (iso, native).

:- comment(catch(Goal, Error, Handler), "Executes @var{Goal}.  If an
   exception is raised during its execution, @var{Error} is unified with
   the exception, and if the unification succeeds, the entire execution
   derived from @var{Goal} is aborted, and @var{Handler} is executed.
   The execution resumes with the continuation of the catch/3 call.  For
   example, given the code
@begin{verbatim}
p(X) :- throw(error), display('---').
p(X) :- display(X).
@end{verbatim}
   the execution of ""@tt{catch(p(0), E, display(E)), display(.), fail.}""
   results in the output ""@tt{error.}"".").

catch(Goal, Error, _) :-
        '$metachoice'(Choice),
        asserta_catching(Choice, Error, []),
        '$metachoice'(BeforeChoice),
        '$meta_call'(Goal),
	'$metachoice'(AfterChoice),
        retract_catching(Choice, Error, []),
        ( BeforeChoice = AfterChoice -> % no more solutions
            ! % remove the unnecessary exception choice point
        ; true
        ).
catch(_, Error, Handler) :-
        retract_fact_nb(thrown(Error)), !,
        '$meta_call'(Handler).

:- true pred intercept(+callable,?term,+callable).

:- comment(intercept(Goal, Error, Handler), "Executes @var{Goal}.  If an
   exception is raised during its execution, @var{Error} is unified with
   the exception, and if the unification succeeds, @var{Handler} is
   executed and then the execution resumes after the predicate which
   produced the exception.  Note the difference with builtin
   @pred{catch/3}, given the same code defined there, the execution of
   ""@tt{intercept(p(0), E, display(E)), display(.), fail.}"" results in
   the output ""@tt{error---.0.}""."). 

intercept(Goal, Error, Handler) :-
        '$metachoice'(Choice),
        asserta_catching(Choice, Error, Handler),
        '$metachoice'(BeforeChoice),
        '$meta_call'(Goal),
	'$metachoice'(AfterChoice),
        retract_catching(Choice, Error, Handler),
        ( BeforeChoice = AfterChoice -> % no more solutions
            ! % remove the unnecessary exception choice point
        ; true
        ).

:- true pred throw(nonvar) + iso.

:- comment(throw(Ball), "Raises an error, throwing the exception
   @var{Ball}, to be caught by an ancestor @pred{catch/3} or
   @pred{intercept/3}.  The closest matching ancestor is chosen.
   Exceptions are also thrown by other builtins in case of error.").

throw(Error) :-
        var(Error), !,
        throw(error(instantiation_error, throw/1-1)).
throw(Error) :-
        current_fact_nb(catching(C, E, H)),
        E = Error, !,
        throw_action(H, E, C).
throw(Error) :-
        display(user_error, '{ERROR: No handle found for thrown error '),
        display(user_error, Error),
        display(user_error, '}'),
        nl(user_error),
        abort.

throw_action([], Error, Choice) :-
        asserta_fact(thrown(Error)),
        cut_to(Choice), % This cuts also next clause
        fail.
throw_action(Handler, Error, Choice) :-
        retract_catching(Choice, Error, Handler),
        '$meta_call'(Handler),
        asserta_catching(Choice, Error, Handler).

cut_to(Choice) :-
        retract_fact_nb(catching(C,_,_)),
        C = Choice,
        '$metacut'(Choice).

asserta_catching(Ch, Er, Ha) :- asserta_fact(catching(Ch, Er, Ha)).
asserta_catching(Ch, Er, Ha) :- retract_fact_nb(catching(Ch, Er, Ha)), fail.

retract_catching(Ch, Er, Ha) :- retract_fact_nb(catching(Ch, Er, Ha)).
retract_catching(Ch, Er, Ha) :- asserta_fact(catching(Ch, Er, Ha)), fail.
