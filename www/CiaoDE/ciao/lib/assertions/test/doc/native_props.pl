:- module(native_props,
        [  linear/1
         , indep/2
         , indep/1
         , mshare/1
         , fails/1
         , not_fail/1
         , possible_fail/1
         , covered/1
         , not_covered/1 
         , is_det/1
         , possible_nondet/1
         , disjoint/1
         , not_disjoint/1
         , lower_size/2
         , upper_size/2
         , lower_time/2 
         , upper_time/2  
        ],
        [assertions]).

:- comment(title,"Properties which are native to analyzers").

:- comment(author,"Francisco Bueno").
:- comment(author,"Manuel Hermenegildo").
:- comment(author,"Pedro Lopez").

:- comment(module,"@cindex{properties, native} This library contains a
   set of properties which are natively understood by the different
   analyzers of @apl{ciaopp}.  They are used by @apl{ciaopp} on output
   and they can also be used in as properties in assertions.").

:- comment(linear(X), "@var{X} is bound to a term which is linear,
   i.e., if it contains any variables, such variables appear only once
   in the term. For example, @tt{[1,2,3]} and @tt{f(A,B)} are linear
   terms, while @tt{f(A,A)} is not.").

:- prop linear(X)
# "@var{X} is instantiated to a linear term.".

linear(_).

:- comment(indep(X,Y), "@var{X} and @var{Y} are @index{independent},
   i.e., they are bound to terms which have no variables in
   common. For example, @tt{indep(X,Y)} holds for @tt{X=f(Z),Y=g(K)}
   and also for @tt{X=f(a),Y=X} (since both @tt{X} and @tt{Y} are
   bound to ground terms). It does not hold for @tt{X=f(Z),Y=g(Z)} and
   for @tt{X=Y}.").

:- prop indep(X,Y) 
# "@var{X} and @var{Y} do not have variables in common.".

indep(_, _).
 
 %% :- prop indep(X,Y) 
 %% # "The terms to which @var{X} and @var{Y} are bound do not have
 %%    variables in common.".

:- comment(indep(X), "The variables in the list @tt{@var{X}} are
pairwise independent.").
 
:- prop indep(X) 
# "The variables in @tt{@var{X}} are pairwise independent.".

indep([]).
indep([[X,Y]|L]):- indep(X,Y), indep(L).

:- comment(mshare(X), "@var{X} contains all @index{sharing sets}
   @cite{jacobs88,abs-int-naclp89} which specify the possible variable
   occurrences in the terms to which the variables involved in the
   clause may be bound. Sharing sets are a compact way of representing
   groundness of variables and dependencies between variables. This
   representation is however generally difficult to read for
   humans. For this reason, this information is often translated to
   @prop{ground/1}, @prop{indep/1} and @prop{indep/2} properties,
   which are easier to read.").

:- prop mshare(X) 
# "The sharing pattern is @tt{@var{X}}.".

mshare(_).


:- comment(fails(X), "Calls of the form @var{X} fail.").

:- prop fails(X)
# "Calls of the form @var{X} fail.".

fails(_).

:- comment(not_fail(X), "Calls of the form @var{X} produce at least
  one solution, or not terminate @cite{non-failure-iclp97}.").

:- prop not_fail(X)
# "All the calls of the form @var{X} do not fail.".

not_fail(_).

:- comment(possible_fail(X), "Non-failure is not ensured for any call
of the form @var{X} @cite{non-failure-iclp97}. In other words, nothing
can be ensured about non-failure nor termination of such calls.").

:- prop possible_fail(X)
# "Non-failure is not ensured for calls of the form @var{X}.".

possible_fail(_).

:- comment(covered(X), "For any call of the form @var{X} there is at
least one clause whose test succeeds (i.e. all the calls of the form
@var{X} are covered.) @cite{non-failure-iclp97}.").

:- prop covered(X) 
# "All the calls of the form @var{X} are covered.".

covered(_).

:- comment(not_covered(X), "There is some call of the form @var{X} for
which there is not any clause whose test succeeds
@cite{non-failure-iclp97}.").

:- prop not_covered(X) 
# "Not all of the calls of the form @var{X} are covered.".

not_covered(_).


:- comment(is_det(X), "All calls of the form @var{X} are
deterministic, i.e. produce at most one solution, or not terminate.").

:- prop is_det(X)
# "All calls of the form @var{X} are deterministic.".

is_det(_).

:- comment(possible_nondet(X), "Non-determinism is not ensured for all
calls of the form @var{X}. In other words, nothing can be ensured
about determinacy nor termination of such calls.").

:- prop possible_nondet(X)
# "Non-determinism is not ensured for calls of the form @var{X}.".

possible_nondet(_).

%% disjoint(X)
%% # "Calls of the form @var{X} select at most one clause.".

:- comment(disjoint(X), "For any call of the form @var{X} at most one
clause succeeds, i.e. clauses are pairwise exclusive.").

:- prop disjoint(X)
# "For any call of the form @var{X} at most one clause succeeds.".

disjoint(_).

:- comment(not_disjoint(X), "Not for all calls of the form @var{X} at
most one clause succeeds. I.e. clauses are not disjoint for some
call.").

 %% For any call of the form @var{X} at most one
 %% clause succeeds, i.e. clauses are pairwise exclusive.").

:- prop not_disjoint(X)
# "Not for all calls of the form @var{X} at most one clause
  succeeds.".

not_disjoint(_).

:- comment(lower_size(X, Y), "The minimum size of the terms to which
the argument @var{Y} is bound to is given by the expression
@var{Y}. Various measures can be used to determine the size of an
argument, e.g., list-length, term-size, term-depth, integer-value,
etc. @cite{caslog}.").

:- prop lower_size(X,Y)
# "@var{Y} is a lower bound on the size of argument @var{X}.".

lower_size(_, _).

:- comment(upper_size(X, Y), "The maximum size of the terms to which
the argument @var{Y} is bound to is given by the expression
@var{Y}. Various measures can be used to determine the size of an
argument, e.g., list-length, term-size, term-depth, integer-value,
etc. @cite{caslog}.").

:- prop upper_size(X,Y)
# "@var{Y} is a upper bound on the size of argument @var{X}.".

upper_size(_, _).

%% upper_size(X,Y)
%% # "The maximum size of arguments of calls of the form @var{X} are
%%    given by the expression @var{Y}.".

:- comment(lower_time(X, Y), "The minimum computation time (in
resolution steps) spent by any call of the form @var{X} is given by
the expression @var{Y} @cite{low-bounds-ilps97,granularity-jsc}").

:- prop lower_time(X,Y) 
# "@var{Y} is a lower bound on the cost of any call of the form
@var{X}.".

lower_time(_, _).

%% lower_time(X,Y)
%% # "The minimum computation time spent by calls of the form @var{X} is
%%    given by the expression @var{Y}.".

:- comment(upper_time(X, Y), "The maximum computation time (in
resolution steps) spent by any call of the form @var{X} is given by
the expression @var{Y} @cite{caslog,granularity-jsc}").

:- prop upper_time(X,Y) 
# "@var{Y} is a upper bound on the cost of any call of the form
@var{X}.".

upper_time(_, _).

%% upper_time(X,Y)
%% # "The maximum computation time spent by calls of the form @var{X} is
%%    given by the expression @var{Y}.".
