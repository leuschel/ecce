:- use_package(assertions).
:- comment(nodoc,assertions).
:- comment(title, "Constraint programming over rationals").
:- comment(author, "Christian Holzbaur").
:- comment(author, "Daniel Cabeza").
:- comment(author, "Samir Genaim (Meta-programming predicates)").

:- comment(module, "

@bf{Note:} This package is currently being adapted to the new
characteristics of the Ciao module system. This new version now works
right now to some extent, but it is under further development at the
moment. Use with (lots of) caution.

").

:- comment(bug, "clp(Q) and clp(R) cannot be used simultaneously in
the same program, or even within the same toplevel session.").

:- comment(appendix, "

@subsection{Some CLP(Q) examples}

@noindent
(Other examples can be found in the source and library directories.)

@begin{itemize}
@item 'Reversible' Fibonacci (clpq):
@end{itemize}

@noindent
@begin{verbatim}
@includeverbatim{examples/fib_q}
@end{verbatim}


@begin{itemize}
@item Matrix multiplication (clpq):
@end{itemize}

@noindent
@begin{verbatim}
@includeverbatim{examples/mmatrix_q}
@end{verbatim}


@begin{itemize}
@item Queens (clpq):
@end{itemize}

@noindent
@begin{verbatim}
@includeverbatim{examples/nqueens_q}
@end{verbatim}


@subsection{Meta-programming with CLP(Q)}

The implementation of CLP(Q) in Ciao compiles the constraints in the program to
a sequence of calls to the underlying constraints solver (at compile-time),
this results in efficient implementation, since the structure of the
constraints is processed only at compile-time, but requires the constraints to
be know at static time which can be a limitation for meta-programming bases
applications such as static program analyzers. For example, the call

@begin{verbatim}
  ?- X=(A+B), Y=(C-D), X .>. Y.

  no
@end{verbatim}

fails because @var{X} @pred{.>.} @var{Y} is translated first to a sequence of
calls that require (when they invoked) @var{X} and @var{Y} to be either number
or free variables. To overcome this limitation, you can use @pred{clp_meta/1}
which delays the translation of the constraints from compile-time to run-time
(i.e., when @pred{clp_meta/1} is called), For example

@begin{verbatim}

?- X=(A+B),Y=(C-D), clp_meta([X .>. Y]).

X = A+B,
Y = C-D,
C.<.D+A+B ?

@end{verbatim}

Another operations on constraints which are extensively used in
meta-programming, in particular in static program analysis, are @em{projection}
and @em{entailment check}. The projection operation restricts the constraints
(that are available in the store) to a given set of variables and turns the
answer into terms. You can use the predicates @pred{dump/3} for that purpose:

@begin{verbatim}

?- A .>. C, C .>. B, dump([A,B],[X,Y],Cs).

Cs = [X.>.Y],
C.>.B,
C.<.A ?

?- C=(B+D), clp_meta([A .>. C, D .>. 0]), dump([A,B],[X,Y],Cs).

C = B+D,
Cs = [Y.<.X],
D.<. -B+A,
D.>.0 ?

@end{verbatim}

The @em{entailment check} is used to check if a list of constrains is entailed
by the store. You can use the predicates @pred{clp_entailed/1} for that
purpose:

@begin{verbatim}

?- A .>. C, C .>. B, B .>. D, clp_entailed([ A .>. B, A .>. D]).

B.>.D,
C.>.B,
C.<.A ?

yes

?- A .>=. B, clp_entailed([ A .>. B ]).

no

@end{verbatim}

").


%% @begin{itemize}
%% @item Critical (cpm critical path routine, clpq):
%% @end{itemize}
%% 
%% @noindent
%% @begin{verbatim}
%% @includeverbatim{examples/critical}
%% @end{verbatim}
