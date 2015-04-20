:- use_package(assertions).
:- comment(nodoc,assertions).

:- comment(title,"Functional notation").

:- comment(author,"Daniel Cabeza").

:- comment(module, "This library package allows the use of functional
   notation in a Ciao module/program.

   It should be made clear that this package just provides a kind of
   @em{syntactic sugar} for defining and using predicates as if they
   were functions, and thus any function definition is in fact defining
   a predicate, and any predicate can be used as a function.  The
   predicate associated to a function has the same name and one more
   argument, to hold the result of the function.  This argument is by
   default added to the right, i.e., it is the last argument, but can be
   defined otherwise by using the appropriate declaration.

   Any term preceded by the @tt{~} operator is a function application,
   as can be seen in the goal @tt{write(~arg(1,T))}, which is equivalent
   to the sequence @tt{arg(1,T,A), write(A)}. To use a predicate
   argument other than the last as the return argument, a declaration
   like @tt{:- fun_return functor(~,_,_)} can be used, so that
   @tt{~functor(f,2)} is evaluated to the term @tt{f(_,_)}.  This
   definition of the return argument can also be done on the fly in each
   invocation in the following way: @tt{~functor(~,f,2)}.

   Functors can be declared as evaluable by using the declaration
   @pred{function/1}. This allows avoiding the need to use the @tt{~}
   operator. Thus, @tt{:- function arg/2} allows writing
   @tt{write(arg(1,T))} instead of @tt{write(~arg(1,T))} as above. This
   declaration can be combined with the previous one: @tt{:- function
   functor(~,_,_)}.  Note that all these declarations, as is customary
   in Ciao, are local to the module where they are included. Finally,
   the @tt{~} operator does not need to be used within a functional
   definition (see below) for the functor being defined.

  In addition to functors declared with the declaration
  @pred{function/1}, the package defines several functors as evaluable
  by default, those being:
@begin{itemize}
@item All the functors understood by @pred{is/2}.  This feature can be
      disabled by a declaration @tt{:- function(arith(false))} (and
      reverted by using @tt{true} instead of @tt{false}).
@item The functors used for disjunctive and conditional expressions,
      @tt{(|)/2} and @tt{(?)/2}.  A disjunctive expression has the form
      @tt{(V1|V2)}, and its value when first evaluated is @tt{V1}, and
      on backtracking @tt{V2}.  A conditional expression has the form
      @tt{(Cond ? V1)}, or more commonly @tt{(Cond ? V1 | V2)}, and its
      value, if the execution of @tt{Cond} as a goal succeeds, is
      @tt{V1}, otherwise in the first form it causes backtracking, and
      on the second form its value is @tt{V2}.  Note that due to the
      operator precedences, these expressions need normally to be
      surrounded by parenthesis.
@end{itemize}

   A functional clause is written using the binary operator @tt{:=},
   as in
@begin{verbatim}
opposite(red) := green.
@end{verbatim}
   Functional clauses can also have a body, which is executed before the
   result value is computed.  It can serve as a guard for the clause or to
   provide the equivalent of where-clauses in functional languages:
@begin{verbatim}
fact(0) := 1.
fact(N) := N * fact(--N) :- N > 0.
@end{verbatim}
   In the return expression of a functional clause, the defined function
   does not need to be preceded by @tt{~}.  Note that guards can often
   be defined more compactly using conditional expressions:
@begin{verbatim}
fact(N) := N = 0 ? 1
         | N > 0 ? N * fact(--N).
@end{verbatim}

   In clause heads (independently of whether they are defined as
   predicates or functions) functors can be prevented from being
   evaluated by using the @tt{(^)/1} prefix operator, as in
@begin{verbatim}
pair(A,B) := ^(A-B).
@end{verbatim}
   Note that this just prevents the evaluation of the principal functor
   of the enclosed term, not the possible occurrences of other evaluable
   functors inside.  The operator is by now ignored outside clause
   heads, due to the recurrent nature of the goal translations used.

   When using function applications inside the goal arguments of
   meta-predicates, there is an ambiguity as they could be evaluated
   either in the scope of the outer execution or the in the scope of
   the inner execution.  The chosen behavior is by default to evaluate
   function applications in the scope of the outer execution, and if
   they should be evaluated in the inner scope, the goal containing the
   function application needs to be escaped with the @tt{(^^)/1} prefix
   operator, as in @tt{findall(X, (d(Y), ^^(X = ~f(Y)+1)), L)} (which could
   also be written as @tt{findall(X, ^^ (d(Y), X = ~f(Y)+1), L)}).

   The translation of functional clauses defining recursive predicates
   maintains the tail recursion of the equivalent predicate, thus allowing
   the usual compiler optimizations.
  ").

:- comment(bug, "The @tt{(^)/1} operator only works in clause heads.").
:- comment(bug, "Assumes that @pred{is/2} is imported.").
:- comment(bug, "Lazy functions declarations require translation priorities
   to move it to the lazy package.").

