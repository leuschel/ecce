:- use_package([assertions,regtypes]).
:- comment(nodoc,assertions).
:- comment(nodoc,regtypes).

:- comment(title,"Fuzzy Prolog").  

:- comment(author, "Claudio Vaucheret").
:- comment(author, "Sergio Guadarrama").

:- comment(module, "This package impements an extension of prolog to
deal with uncertainty. We implement a fuzzy prolog that models
interval-valued fuzzy logic. This approach is more general than other
fuzzy prologs in two aspects:

@begin{enumerate}
@item Truth values are sub-intervals on [0,1]. In fact, it could
  be a finite union of sub-intervals, as we will see below. Having a
  unique truth value is a particular case modeled with a unitary
  interval.
@item Truth values are propagated through the rules by means of a
  set of @em{aggregation operators}. The definition of an @em{aggregation
  operator} is a generalization that subsumes conjunctive operators
  (triangular norms as min, prod, etc.), disjunctive operators
  (triangular co-norms as max, sum, etc.), average operators
  (averages as arithmetic average, cuasi-linear  average, etc.) and
  hybrid operators (combinations of previous operators).
@end{enumerate}

We add uncertainty using CLP(R) instead of implementing a new fuzzy
resolution as other fuzzy prologs. In this way, we use the original
inference mechanism of Prolog, and we use the constraints and its
operations provided by CLP(R) to handle the concept of partial
truth. We represent intervals as constrains over real numbers and
@em{aggregation operators} as operations with constraints.

Each fuzzy predicate has an additional argument which represents its
truth value. We use ``:~'' instead of ``:-'' to distinguish fuzzy
clauses from prolog clauses. We have implemented the following
aggregation operators:




Another example is:

@begin{verbatim}
@includeverbatim{examples/dicesum5.pl}
@end{verbatim}

").

:- include(library('fuzzy/ops')).

:- true pred :#(Name, Decl) : predname * fuzzydecl
        # "Defines fuzzy predicate @var{Name} from the declaration
          @var{Decl}.".
:- impl_defined((:#)/2).

:- comment(hide,fuzzydecl/1).
:- regtype fuzzydecl/1
	# "One of the following three: @includedef{fuzzydecl/1}".

fuzzydecl(fuzzy_predicate(_)).
fuzzydecl(fuzzy(_)).
fuzzydecl(fnot(_)).

:- true pred fuzzy_predicate(Domain) : list
        # "Defines a fuzzy predicate with piecewise linear continuous
           membership function. This is given by @var{Domain}, which
           is a list of pairs of domain-truth values, in increasing order
           and exhaustive. For example:
@begin{verbatim}
young :# fuzzy_predicate([(0,1),(35,1),(45,0),(120,0)]).
@end{verbatim}
           defines the predicate:
@begin{verbatim}
young(X,1):- X .>=. 0, X .<. 35.
young(X,M):- X .>=. 35, X .<. 45, 10*M .=. 45-X.
young(X,0):- X .>=. 45, X .=<. 120. 
@end{verbatim}
".
:- impl_defined(fuzzy_predicate/1).

:- true pred fuzzy(Name) : predname
        # "Defines a fuzzy predicate as the fuzzy counterpart of a crisp
           predicate @var{Name}. For example,
@begin{verbatim}
p_f :# fuzzy p/2
@end{verbatim}
           defines a new fuzzy predicate @tt{p_f/3} (the last
           argument is the truth value) with truth value equal to 0 
           if @tt{p/2} fails and 1 otherwise.".
:- impl_defined(fuzzy/1).

:- true pred fnot(Name) : predname
        # "Defines a fuzzy predicate as the fuzzy negation of another
           fuzzy predicate @var{Name}. For example,
@begin{verbatim}
notp_f :# fnot  p_f/3
@end{verbatim}
           defines the predicate:
@begin{verbatim}
notp_f(X,Y,M) :-
        p_f(X,Y,Mp),
        M .=. 1 - Mp.
@end{verbatim}
".
:- impl_defined(fnot/1).

:- true pred (Head :~ Body) : callable * fuzzybody
        # "Defines a fuzzy clause for a fuzzy predicate. The clause contains
           calls to either fuzzy or crisp predicates. Calls to crisp
           predicates are automatically fuzzified. The last argument of
           @var{Head} is the truth value of the clause, which is obtained
           as the aggregation of the truth values of the body goals. In
           the absence of an explicit aggregator, @var{min} is used.
           An example:
@begin{verbatim}
@includeverbatim{examples/young2.pl}
@end{verbatim}
           so that:
@begin{verbatim}
?- young_couple(john,rose,M).

M .=. 0.6 ? 
@end{verbatim}
".
:- impl_defined((:~)/2).

:- prop fuzzybody/1
	# "A clause body, optionally prefixed by the name of an
           aggregation operator. The agregators currently provided are
           listed under @var{faggregator/1}.".
:- impl_defined(fuzzybody/1).

:- regtype faggregator/1
	# "The first three are, respectively, the T-norms: minimum,
           product, and Lukasiewicz's. The last three are their corresponding
           T-conorms.".

faggregator(min).
faggregator(prod).
faggregator(luka).
faggregator(max).
faggregator(dprod).
faggregator(dluka).
