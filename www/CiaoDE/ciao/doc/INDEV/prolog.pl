
:- module(prolog,[main/0],[assertions]).

:- comment(title,"A Brief Introduction to Prolog").

:- comment(module,"

Prolog is a simple but powerful programming language developed at the
University of Marseilles (@cite{Roussel75}), as a practical tool for
@concept{programming in logic} (@cite{kowalski74-tr}).  From a user's
point of view the major attraction of the language is ease of
programming.  Clear, readable, concise programs can be written quickly
with few errors.

@cindex{logic programming}
@cindex{programming in logic}

For an introduction to programming in Prolog, readers are recommended
to consult @cite{Sterling}.  For a more general introduction to the
field of Logic Programming see @cite{Kowalski79}. Recently, the core
of the Prolog language has been standardized by the International
Standards Organization (@concept{ISO}). A full description of this
@concept{ISO-Prolog} standard can be found in @cite{iso-prolog}.
However, for the benefit of those who do not have access to a copy of
this book, and for those who have some prior knowledge of logic
programming, a summary of the language is included below.

@node Prolog Intro, Example Intro, Built Intro, Top
@comment  node-name,  next,  previous,  up
@chapter The Prolog Language

@cindex defininte clause
@cindex Horn clause
This chapter provides a brief introduction to the syntax and semantics of
a certain subset of logic (@dfn{definite clauses}, also known as @dfn{Horn
clauses}), and indicates how this subset forms the basis of Prolog.

@section Syntax, Terminology and Informal Semantics

@subsection Terms

The data objects of the language are called @dfn{terms}.  A term is either
a @dfn{constant}, a @dfn{variable} or a @dfn{compound term}.

@cindex integer
The constants include @dfn{integers} such as

@example
0   1   999   -512
@end example

Besides the usual decimal, or base 10, notation, integers may also be
written in any base from 2 to 36, of which base 2 (binary), 8 (octal),
and 16 (hex) are probably the most useful.  Letters @kbd{A} through
@kbd{Z} (upper or lower case) are used for bases greater than 10.
E.g.

@example
15   2'1111   8'17    16'F
@end example

@noindent
all represent the integer fifteen.

There is also a special notation for character constants.  E.g.

@example
0'A
@end example

@noindent
is equivalent to @code{65} (the numerical value of the ASCII code for
@kbd{A}). @refill

@cindex float
Constants also include @dfn{floats} such as

@example
1.0   -3.141   4.5E7   -0.12e+8   12.0e-9
@end example

Note that there must be a decimal point in floats written with an exponent,
and that there must be at least one digit before and after the decimal point.

@cindex atom
Constants also include @dfn{atoms} such as

@example
a   void   =   :=   'Algol-68'   []
@end example

Constants are definite elementary objects, and correspond to proper nouns
in natural language.  For reference purposes, here is a list of the
possible forms which an atom may take:

@enumerate

@item
Any  sequence  of  alphanumeric characters (including @kbd{_}), starting
with a lower case letter. @refill

@item
Any sequence from the following set of characters:@*
@kbd{+}@kbd{-}@kbd{*}@kbd{/}@kbd{\}@kbd{^}@kbd{<}@kbd{>}@kbd{=}@kbd{`}@kbd{~}@kbd{:}@kbd{.}@kbd{?}@kbd{@@}@kbd{#}@kbd{$}@kbd{&}@*
This set can in fact be larger; @pxref{Token String} for a precise
definition.

@item
Any sequence of characters delimited by single quotes.  If the single quote
character is included in the sequence it must be written twice, e.g.
@code{'can''t'}. @refill

@item
Any of: @kbd{!} @kbd{;} @kbd{[]} @kbd{@{@}}@* 
Note that the bracket pairs are special: @kbd{[]} and @kbd{@{@}} are
atoms but @kbd{[}, @kbd{]}, @kbd{@{}, and @kbd{@}} are not.  However,
when they are used as functors (see below) the form @code{@{@var{X}@}}
is allowed as an alternative to @code{@{@}(@var{X})}.  The form
@code{[@var{X}]} is the normal notation for lists, as an alternative to
@code{.(X,[])}. @refill
@end enumerate

Variables may be written as any sequence of alphanumeric characters
(including @kbd{_}) starting with either a capital letter or @kbd{_}; e.g.

@example
X   Value   A   A1   _3   _RESULT
@end example

@cindex anonymous variable
If a variable is only referred to once in a clause, it does not need to be
named and may be written as an @dfn{anonymous} variable, indicated by the
underline character @kbd{_}.  A clause may contain several anonymous 
variables; they are all read and treated as distinct variables.

A variable should be thought of as standing for some definite but
unidentified object.  This is analogous to the use of a pronoun in natural
language.  Note that a variable is not simply a writeable storage location
as in most programming languages; rather it is a local name for some data
object, cf.  the variable of pure LISP and identity declarations in
Algol68.

@cindex functor
@cindex arity
The structured data objects of the language are the compound terms.  A
compound term comprises a @dfn{functor} (called the principal functor of
the term) and a sequence of one or more terms called @dfn{arguments}.  A
functor is characterised by its name, which is an atom, and its @dfn{arity}
or number of arguments.  For example the compound term whose functor is
named @code{point} of arity 3, with arguments @code{X}, @code{Y} and
@code{Z}, is written @refill

@example
point(X, Y, Z)
@end example

Note that an atom is considered to be a functor of arity 0.

Functors are generally analogous to common nouns in natural language.  One
may think of a functor as a record type and the arguments of a compound
term as the fields of a record.  Compound terms are usefully pictured as
trees.  For example, the term

@example
s(np(john),vp(v(likes),np(mary)))
@end example

@noindent
would be pictured as the structure

@smallexample
       s
     /   \
  np       vp
  |       /  \
john     v     np
         |     |
       likes  mary
@end smallexample

Sometimes it is convenient to write certain functors as operators---2-ary
functors may be declared as infix operators and 1-ary functors as prefix or
postfix operators.  Thus it is possible to write, e.g.

@example
X+Y     (P;Q)     X<Y      +X     P;
@end example

@noindent
as optional alternatives to 

@example
+(X,Y)   ;(P,Q)   <(X,Y)   +(X)   ;(P)
@end example

@noindent
The use of operators is described fully below (@pxref{Operators}).

Lists form an important class of data structures in Prolog.  They are
essentially the same as the lists of LISP: a list either is the atom
@code{[]} representing the empty list, or is a compound term with
functor @code{.} and two arguments which are respectively the head and
tail of the list.  Thus a list of the first three natural numbers is the
structure @refill

@smallexample
  .
 / \
1    .
    / \
   2    .
       / \
      3   []
@end smallexample

@noindent
which could be written, using the standard syntax, as 

@example
.(1,.(2,.(3,[])))
@end example

@noindent
but which is normally written, in a special list notation, as 

@example
[1,2,3]
@end example

@noindent
The special list notation in the case when the tail of a list is a variable
is exemplified by

@example
[X|L]     [a,b|L]
@end example

@noindent
representing

@smallexample
   .               .
  / \             / \
X     L         a     .
                     / \
                   b     L
@end smallexample

@noindent
respectively.

Note that this notation does not add any new power to the language; it
simply makes it more readable.  e.g. the above examples could equally be
written

@example
.(X,L)    .(a,.(b,L))
@end example

@cindex string
For convenience, a further notational variant is allowed for lists of
integers which correspond to ASCII character codes.  Lists written in this
notation are called @dfn{strings}.  E.g.

@example
"SICStus"
@end example

@noindent
which represents exactly the same list as 

@example
[83,73,67,83,116,117,115]
@end example

@node Programs,  , Terms, Syntax
@comment  node-name,  next,  previous,  up
@subsection Programs
@cindex program
@cindex goal

A fundamental unit of a logic program is the @dfn{goal} or procedure call.
E.g. @refill

@example
gives(tom, apple, teacher)   reverse([1,2,3], L)   X<Y
@end example

@cindex predicate
A goal is merely a special kind of term, distinguished only by the
context in which it appears in the program.  The (principal) functor of
a goal identifies what @dfn{predicate} the goal is for.  It corresponds
roughly to a verb in natural language, or to a procedure name in a
conventional programming language. @refill

@cindex sentence
@cindex clause
@cindex head
@cindex body
A logic program consists simply of a sequence of statements called
@dfn{sentences}, which are analogous to sentences of natural language.  A
sentence comprises a @dfn{head} and a @dfn{body}.  The head either consists
of a single goal or is empty.  The body consists of a sequence of zero or
more goals (i.e. it too may be empty).  If the head is not empty, the
sentence is called a @dfn{clause}. @refill

@cindex unit clause
If the body of a clause is empty, the clause is called a @dfn{unit
clause}, and is written in the form

@example
@var{P}.
@end example

@noindent
where @var{P} is the head goal.  We interpret this declaratively as

@quotation
@var{P} is true.
@end quotation

@noindent
and procedurally as 

@quotation
Goal @var{P} is satisfied.
@end quotation

@cindex non-unit clause
If the body of a clause is non-empty, the clause is called a
@dfn{non-unit clause}, and is written in the form

@example
@var{P} :- @var{Q}, @var{R}, @var{S}.
@end example

@noindent
where @var{P} is the head goal and @var{Q}, @var{R} and @var{S} are the
goals which make up the body.  We can read such a clause either
declaratively as @refill

@quotation
@var{P} is true if @var{Q} and @var{R} and @var{S} are true.
@end quotation

or procedurally as

@quotation
To satisfy goal @var{P}, satisfy goals @var{Q}, @var{R} and @var{S}.
@end quotation

A sentence with an empty head is called a @dfn{directive}
(@pxref{Directives}), of which the most important kind is called a
@dfn{query} and is written in the form @refill

@example
?- @var{P}, @var{Q}.
@end example

@noindent
where @var{P} and @var{Q} are the goals of the body.  Such a query is
read declaratively as

@quotation
Are @var{P} and @var{Q} true?
@end quotation

@noindent
and procedurally as

@quotation
Satisfy goals @var{P} and @var{Q}.
@end quotation

Sentences generally contain variables.  Note that variables in different
sentences are completely independent, even if they have the same
name---i.e. the @dfn{lexical scope} of a variable is limited to a
single sentence.  Each distinct variable in a sentence should be
interpreted as standing for an arbitrary entity, or value.  To
illustrate this, here are some examples of sentences containing
variables, with possible declarative and procedural readings:

@enumerate

@item
@code{employed(@var{X}) :- employs(@var{Y},@var{X}).}

``Any @var{X} is employed if any @var{Y} employs @var{X}.''

``To find whether a person @var{X} is employed,
find whether any @var{Y} employs @var{X}.'' @refill

@item
@code{derivative(@var{X},@var{X},1).}

``For any @var{X}, the derivative of @var{X} with respect to @var{X}
is 1.'' @refill

``The goal of finding a derivative for the expression @var{X} with
respect to @var{X} itself is satisfied by the result 1.'' @refill

@item
@code{?- ungulate(@var{X}), aquatic(@var{X}).}

``Is it true, for any @var{X}, that @var{X} is an ungulate and @var{X} is
aquatic?'' @refill

``Find an @var{X} which is both an ungulate and aquatic.''
@end enumerate

@cindex predicate
In any program, the @dfn{predicate} for a particular (principal) functor
is the sequence of clauses in the program whose head goals have that
principal functor.  For example, the predicate for a 3-ary functor
@code{concatenate/3} might well consist of the two clauses @refill

@example
concatenate([], L, L).
concatenate([X|L1], L2, [X|L3]) :- concatenate(L1, L2, L3).
@end example

@noindent
where @code{concatenate(@var{L1},@var{L2},@var{L3})} means ``the list
@var{L1} concatenated with the list @var{L2} is the list @var{L3}''.
Note that for predicates with clauses corresponding to a base case and a
recursive case, the preferred style is to write the base case clause
first.

In Prolog, several predicates may have the same name but different
arities.  Therefore, when it is important to specify a predicate
unambiguously, the form @code{@var{name}/@var{arity}} is used; e.g.
@code{concatenate/3}.

@cindex built-in predicate
Certain predicates are predefined by built-in predicates supplied by the
Prolog system.  Such predicates are called @dfn{built-in predicates}.

As we have seen, the goals in the body of a sentence are linked by the
operator @samp{,} which can be interpreted as conjunction (``and'').  It
is sometimes convenient to use an additional operator @samp{;}, standing
for disjunction (``or'').  (The precedence of @samp{;} is such that it
dominates @samp{,} but is dominated by @samp{:-}.)  An example is the
clause @refill

@example
grandfather(X, Z) :-
        (mother(X, Y); father(X, Y)),
        father(Y, Z).
@end example

@noindent
which can be read as 

@quotation
For any @var{X}, @var{Y} and @var{Z}, @var{X} has @var{Z} as a
grandfather if either the mother of @var{X} is @var{Y} or the father of
@var{X} is @var{Y}, and the father of @var{Y} is @var{Z}.
@end quotation

Such uses of disjunction can always be eliminated by defining an extra
predicate---for instance the previous example is equivalent to

@example
grandfather(X,Z) :- parent(X,Y), father(Y,Z).

parent(X,Y) :- mother(X,Y).
parent(X,Y) :- father(X,Y).
@end example

@noindent
---and so disjunction will not be mentioned further in the following,
more formal, description of the semantics of clauses.

The token @samp{|}, when used outside a list, is an alias for @samp{;}.
The aliasing is performed when terms are read in, so that

@example
a :- b | c.
@end example

@noindent
is read as if it were 

@example
a :- b ; c.
@end example

Note the double use of the @samp{.} character.  On the one hand it is
used as a sentence terminator, while on the other it may be used in a
string of symbols which make up an atom (e.g. the list functor
@code{./2}).  The rule used to disambiguate terms is that a @samp{.}
followed by a @var{layout-char} is regarded as a sentence terminator
(@pxref{Token String}).

@node Declarative, Procedural, Syntax, Prolog Intro
@comment  node-name,  next,  previous,  up
@section Declarative Semantics
@cindex declarative semantics
@cindex semantics

The semantics of definite clauses should be fairly clear from the
informal interpretations already given.  However it is useful to have a
precise definition.  The @dfn{declarative semantics} of definite clauses
tells us which goals can be considered true according to a given
program, and is defined recursively as follows.

@quotation
A goal is true if it is the head of some clause instance and each of the
goals (if any) in the body of that clause instance is true, where an
instance of a clause (or term) is obtained by substituting, for each of
zero or more of its variables, a new term for all occurrences of the
variable.
@end quotation

For example, if a program contains the preceding predicate for
@code{concatenate/3}, then the declarative semantics tells us that
@refill

@example
?- concatenate([a], [b], [a,b]).
@end example

@noindent
is true, because this goal is the head of a certain instance of the
first clause for @code{concatenate/3}, namely,

@example
concatenate([a], [b], [a,b]) :- concatenate([], [b], [b]).
@end example

@noindent
and we know that the only goal in the body of this clause instance is
true, since it is an instance of the unit clause which is the second
clause for @code{concatenate/3}.

@node Procedural, Occur, Declarative, Prolog Intro
@comment  node-name,  next,  previous,  up
@section Procedural Semantics
@cindex procedural semantics
@cindex semantics

Note that the declarative semantics makes no reference to the sequencing
of goals within the body of a clause, nor to the sequencing of clauses
within a program.  This sequencing information is, however, very
relevant for the @dfn{procedural semantics} which Prolog gives to
definite clauses.  The procedural semantics defines exactly how the
Prolog system will execute a goal, and the sequencing information is the
means by which the Prolog programmer directs the system to execute the
program in a sensible way.  The effect of executing a goal is to
enumerate, one by one, its true instances.  Here then is an informal
definition of the procedural semantics.  We first illustrate the
semantics by the simple query

@example
?- concatenate(@var{X}, @var{Y}, [a,b]).
@end example

@noindent
We find that it matches the head of the first clause for
@code{concatenate/3}, with @var{X} instantiated to @code{[a|@var{X1}]}.
The new variable @var{X1} is constrained by the new query produced,
which contains a single recursive procedure call: @refill

@example
?- concatenate(@var{X1}, @var{Y}, [b]).
@end example

@noindent
Again this goal matches the first clause, instantiating @var{X1} to
@code{[b|@var{X2}]}, and yielding the new query:

@example
?- concatenate(@var{X2}, @var{Y}, [])
@end example

@noindent
Now the single goal will only match the second clause, instantiating
both @var{X2} and @var{Y} to @code{[]}.  Since there are no further goals
to be executed, we have a solution @refill

@example
X = [a,b]
Y = []
@end example

@noindent
i.e. a true instance of the original goal is 

@example
concatenate([a,b], [], [a,b])
@end example

@noindent
If this solution is rejected, backtracking will generate the further
solutions

@example
X = [a]
Y = [b]

X = []
Y = [a,b]
@end example

@noindent
in that order, by re-matching, against the second clause for concatenate,
goals already solved once using the first clause.

Thus, in the procedural semantics, the set of clauses

@example
@var{H} :- @var{B1}, ..., @var{Bm}.
@var{H'} :- @var{B1'}, ..., @var{Bm'}.
...
@end example

@noindent
@cindex procedure definition
are regarded as a @dfn{procedure definition} for some predicate @var{H},
and in a query

@example
?- @var{G1}, ..., @var{Gn}.
@end example

@noindent
@cindex procedure call
@cindex computation rule
@cindex search rule
@cindex unification
each @var{Gi} is regarded as a @dfn{procedure call}.  To execute a
query, the system selects by its @dfn{computation rule} a goal, @var{Gj}
say, and searches by its @dfn{search rule} a clause whose head matches
@var{Gj}.  Matching is done by the @dfn{unification} algorithm (see
@cite{A Machine-Oriented Logic Based on the Resolution Principle} by
J.A. Robinson, @cite{Journal of the ACM} 12:23-44, January 1965) which
computes the most general unifier, @dfn{mgu}, of @var{Gj} and @var{H}.
The @var{mgu} is unique if it exists.  If a match is found, the current
query is @dfn{reduced} to a new query @refill

@example
?- (@var{G1}, ..., @var{Gj-1}, @var{B1}, ..., @var{Bm}, @var{Gj+1}, ..., @var{Gn})@var{mgu}.
@end example

@noindent
and a new cycle is started.  The execution terminates when the empty
query has been produced.

@cindex backtracking
If there is no matching head for a goal, the execution @dfn{backtracks}
to the most recent successful match in an attempt to find an alternative
match.  If such a match is found, an alternative new query is produced,
and a new cycle is started.

In SICStus Prolog, as in other Prolog systems, the search rule is
simple: ``search forward from the beginning of the program''.  

@cindex blocking
The computation rule in most Prolog systems is simple too: ``pick the
leftmost goal of the current query''.  However, SICStus Prolog, Prolog
II, NU-Prolog, and a few other systems have a somewhat more complex
computation rule ``pick the leftmost @dfn{unblocked} goal of the current
query''.  A goal is @dfn{blocked on its first argument} if that argument
is uninstantiated and its predicate definition is annotated with a
@dfn{wait declaration} (@pxref{Declarations}).  Goals of the built-in
predicates @code{freeze/1} and @code{dif/2} (q.v.) may also be blocked
if their arguments are not instantiated enough.  A goal can only be
blocked on a single uninstantiated variable, but a variable may block
several goals. @refill

Thus binding a variable can cause blocked goals to become unblocked, and
backtracking can cause currently unblocked goals to become blocked
again.  Moreover, if the current query is

@example
?- @var{G1}, ..., @var{Gj-1}, @var{Gj}, @var{Gj+1}, ..., @var{Gn}.
@end example

@noindent
where @var{Gj} is the first unblocked goal, and matching @var{Gj}
against a clause head causes several blocked goals in @var{G1}, ...,
@var{Gj-1} to become unblocked, then these goals may become reordered.
The internal order of any two goals that were blocked on the @emph{same}
variable is retained, however. @refill

@cindex floundering
Another consequence is that a query may be derived consisting entirely
of blocked goals.  Such a query is said to have @dfn{floundered}.  The
interpreter top-level checks for this condition.  If detected, the
outstanding blocked subgoals are printed on the terminal along with the
answer substitution, to notify the user that the answer (s)he has
got is really a speculative one, since it is only valid if the blocked
goals can be satisfied.

In compiled code, the computation rule is not completely obeyed, as
calls to certain built-in predicates compile to instructions.  Such
calls are executed even if a unification just prior to the call causes a
blocked goal to become unblocked.  The following built-in predicates do
not compile to procedure calls in compiled code.  Note also that there is an
implicit cut in the @code{\+} and @code{->} constructs:

@example
'C'/3
arg/3
atom/1
atomic/1
compare/3
float/1
functor/3
is/2
integer/1
nonvar/1
number/1
var/1
'=='/2 '\=='/2 '@@<'/2 '@@>='/2 '@@>'/2 '@@=<'/2
'=:='/2 '=\='/2 '<'/2 '>='/2 '>'/2 '=<'/2
'=..'/2 '='/2 ','/2 !/0
@end example

Sometimes, it is crucial that the blocked goal be executed before a call
to one of the above built-in predicates.  Since most of the above are
meta-logical primitives, their semantics can depend on whether a
variable is currently bound etc.  Consider, for example, the clauses and
query

@example
:- wait test/1.
test(2).

data(1).
data(2).

?- test(X), data(X), !, ...
@end example

@noindent
thus the first match for @code{data(X)} causes the blocked goal
@code{test(X)} to be unblocked, but since the cut is selected before
@code{test(X)}, the system is committed to the first match for
@code{data(X)}, and the query fails.  However, inserting a dummy goal
@code{true} enables the unblocked goal to be selected before the cut: @refill

@example
?- test(X), data(X), true, !, ...
@end example

@noindent
As @code{test(1)} fails, the system backtracks to the second clause for
@code{data(X)}, and the query succeeds with the answer

@example
X = 2
@end example


@node Occur, Cut, Procedural, Prolog Intro
@comment  node-name,  next,  previous,  up
@section Occurs Check

@cindex occurs check
It is possible, and sometimes useful, to write programs which unify a
variable to a term in which that variable occurs, thus creating a cyclic
term.  The usual mathematical theory behind Logic Programming forbids
the creation of cyclic terms, dictating that an @dfn{occurs check}
should be done each time a variable is unified with a term.
Unfortunately, an occurs check would be so expensive as to render Prolog
impractical as a programming language.  Thus cyclic terms may be created
and may cause loops trying to print them.

SICStus Prolog mitigates the problem by its ability to unify and compare
(@pxref{Term Compare}) cyclic terms without looping.  Loops in the
printer can be interrupted by typing @kbd{^C}.

@node Cut, Operators, Occur, Prolog Intro
@comment  node-name,  next,  previous,  up
@section The Cut Symbol
@findex !/0, cut

@cindex cut
Besides the sequencing of goals and clauses, Prolog provides one other
very important facility for specifying control information.  This is the
@dfn{cut} symbol, written @code{!}.  It is inserted in the program just
like a goal, but is not to be regarded as part of the logic of the
program and should be ignored as far as the declarative semantics is
concerned.

The effect of the cut symbol is as follows.  When first encountered as a
goal, cut succeeds immediately.  If backtracking should later return to
the cut, the effect is to fail the @dfn{parent goal}, i.e. that goal
which matched the head of the clause containing the cut, and caused the
clause to be activated.  In other words, the cut operation
@emph{commits} the system to all choices made since the parent goal was
invoked, and causes other alternatives to be discarded.  The goals thus
rendered @dfn{determinate} are the parent goal itself, any goals
occurring before the cut in the clause containing the cut, and any
subgoals which were executed during the execution of those preceding
goals.

e.g. 

@example
member(X, [X|_]).
member(X, [_|L]) :- member(X, L).
@end example

@noindent
This predicate can be used to test whether a given term is in a list.  E.g.

@example
| ?- member(b, [a,b,c]).
@end example

@noindent
returns the answer @samp{yes}.  The predicate can also be used to extract
elements from a list, as in

@example
| ?- member(X, [d,e,f]).
@end example

@noindent
With backtracking this will successively return each element of the list.
Now suppose that the first clause had been written instead:

@example
member(X, [X|_]) :- !.
@end example

@noindent
In this case, the above call would extract only the first element of the
list (@code{d}).  On backtracking, the cut would immediately fail the whole
predicate.

@example
x :- p, !, q.
x :- r.
@end example

@noindent
This is equivalent to 

@example
x := if p then q else r;
@end example

@noindent
in an Algol-like language.

It should be noticed that a cut discards all the alternatives since the
parent goal, even when the cut appears within a disjunction.  This means
that the normal method for eliminating a disjunction by defining an extra
predicate cannot be applied to a disjunction containing a cut.

A proper use of the cut is usually a major difficulty for new Prolog
programmers.  The usual mistakes are to over-use cut, and to let cuts
destroy the logic.  We would like to advise all users to follow these
general rules.  Also @pxref{Example Intro}.

@itemize @bullet

@item
Write each clause as a self-contained logic rule which just defines the
truth of goals which match its head.  Then add cuts to remove any
fruitless alternative computation paths that may tie up store.

@item
Cuts are usually placed right after the head, sometimes preceded by simple
tests.

@item
Cuts are hardly ever needed in the last clause of a predicate.
@end itemize

@node Operators, Restrictions, Cut, Prolog Intro
@comment  node-name,  next,  previous,  up
@section Operators
@cindex operators

Operators in Prolog are simply a @emph{notational convenience}.  For
example, the expression @code{2+1} could also be written @code{+(2,1)}.
This expression represents the data structure

@smallexample
   +
 /   \
2     1
@end smallexample

@noindent
and @emph{not} the number 3.  The addition would only be performed if the
structure were passed as an argument to an appropriate predicate such as
@code{is/2} (@pxref{Arithmetic}). @refill

The Prolog syntax caters for operators of three main kinds---@dfn{infix},
@dfn{prefix} and @dfn{postfix}.  An infix operator appears between its two
arguments, while a prefix operator precedes its single argument and a
postfix operator is written after its single argument. @refill

Each operator has a precedence, which is a number from 1 to 1200.  The
precedence is used to disambiguate expressions where the structure of
the term denoted is not made explicit through the use of parentheses.
The general rule is that it is the operator with the @emph{highest}
precedence that is the principal functor.  Thus if @samp{+} has a higher
precedence than @samp{/}, then @refill

@example
a+b/c     a+(b/c)
@end example

@noindent
are equivalent and denote the term @code{+(a,/(b,c))}.  Note that the
infix form of the term @code{/(+(a,b),c)} must be written with explicit
parentheses, i.e. @refill

@example
(a+b)/c
@end example

If there are two operators in the subexpression having the same highest
precedence, the ambiguity must be resolved from the types of the
operators.  The possible types for an infix operator are

@example
xfx     xfy     yfx
@end example

Operators of type @code{xfx} are not associative: it is a requirement
that both of the two subexpressions which are the arguments of the
operator must be of @emph{lower} precedence than the operator itself,
i.e. their principal functors must be of lower precedence, unless the
subexpression is explicitly parenthesised (which gives it zero
precedence).

Operators of type @code{xfy} are right-associative: only the first
(left-hand) subexpression must be of lower precedence; the right-hand
subexpression can be of the @emph{same} precedence as the main operator.
Left-associative operators (type @code{yfx}) are the other way around.

@findex op/3
A functor named name is declared as an operator of type @var{Type} and
precedence @var{Precedence} by the command

@example
:- op(@var{Precedence}, @var{Type}, @var{Name}).
@end example

The argument name can also be a list of names of operators of the same
type and precedence.

It is possible to have more than one operator of the same name, so long as
they are of different kinds, i.e. infix, prefix or postfix.  An operator of
any kind may be redefined by a new declaration of the same kind.  This
applies equally to operators which are provided as standard.  Declarations
of all the standard operators can be found elsewhere (@pxref{Standard
Operators}). @refill

For example, the standard operators @code{+} and @code{-} are declared by

@example
:- op(  500, yfx, [ +, - ]).
@end example

@noindent
so that 

@example
a-b+c
@end example

@noindent
is valid syntax, and means 

@example
(a-b)+c
@end example

@noindent
i.e. 

@smallexample
     +
   /   \
  -     c
 / \
a   b
@end smallexample

The list functor @code{.} is not a standard operator, but we could declare it
thus:

@example
:- op(900, xfy, .).
@end example

Then @code{a.b.c} would represent the structure 

@smallexample
  .
 / \
a   .
   / \
  b   c
@end smallexample

Contrasting this with the diagram above for @code{a-b+c} shows the
difference betweeen @code{yfx} operators where the tree grows to the left,
and @code{xfy} operators where it grows to the right.  The tree cannot grow
at all for @code{xfx} operators; it is simply illegal to combine @code{xfx}
operators having equal precedences in this way. @refill

The possible types for a prefix operator are 

@example
fx      fy
@end example

@noindent
and for a postfix operator they are 

@example
xf      yf
@end example

The meaning of the types should be clear by analogy with those for infix
operators.  As an example, if @code{not} were declared as a prefix
operator of type @code{fy}, then

@example
not not P
@end example

@noindent
would be a permissible way to write @code{not(not(P))}.  If the type
were @code{fx}, the preceding expression would not be legal, although
@refill

@example
not P
@end example

@noindent
would still be a permissible form for @code{not(P)}.

If these precedence and associativity rules seem rather complex,
remember that you can always use parentheses when in any doubt.

Note that the arguments of a compound term written in standard syntax
must be expressions of precedence @emph{below} 1000.  Thus it is
necessary to parenthesise the expression @code{P :- Q} in

@example
?- assert((P :- Q)).
@end example

@node Restrictions, Comments, Operators, Prolog Intro
@comment  node-name,  next,  previous,  up
@section Syntax Restrictions
@cindex syntax restrictions

Note carefully the following syntax restrictions, which serve to remove
potential ambiguity associated with prefix operators.

@enumerate

@item
In a term written in standard syntax, the principal functor and its
following @kbd{(} must @emph{not} be separated by any intervening spaces,
newlines etc.  Thus @refill

@example
point (X,Y,Z)
@end example

@noindent
is invalid syntax.

@item
If the argument of a prefix operator starts with a @kbd{(}, this @kbd{(}
must be separated from the operator by at least one space or other
non-printable character.  Thus @refill

@example
:-(p;q),r.
@end example

@noindent
(where @samp{:-} is the prefix operator) is invalid syntax.  The system
would try to interpret it as the structure: @refill

@smallexample
    ,
   / \
 :-    r
  |
  ;
 / \
p   q
@end smallexample

That is, it would take @samp{:-} to be a functor of arity 1.  However,
since the arguments of a functor are required to be expressions of
precedence below 1000, this interpretation would fail as soon as the
@samp{;} (precedence 1100) was encountered. @refill

In contrast, the term:

@example
:- (p;q),r.
@end example

@noindent
is valid syntax and represents the following structure.  

@smallexample
   :-
    |
    ,
   / \
  ;   r
 / \
p   q
@end smallexample
@end enumerate

@node Comments, Full Syntax, Restrictions, Prolog Intro
@comment  node-name,  next,  previous,  up
@section Comments
@comment

Comments have no effect on the execution of a program, but they are very
useful for making programs more readily comprehensible.  Two forms of
comment are allowed in Prolog:

@enumerate

@item
The character @kbd{%} followed by any sequence of characters up to end of
line.

@item
The symbol @kbd{/*} followed by any sequence of characters (including new
lines) up to @kbd{*/}. @refill
@end enumerate

@node Full Syntax,  , Comments, Prolog Intro
@comment  node-name,  next,  previous,  up
@section Full Prolog Syntax

A Prolog program consists of a sequence of @dfn{sentences}.  Each sentence
is a Prolog @dfn{term}.  How terms are interpreted as sentences is defined
below (@pxref{Sentence}).  Note that a term representing a sentence may be
written in any of its equivalent syntactic forms.  For example, the 2-ary
functor @samp{:-} could be written in standard prefix notation instead of
as the usual infix operator. @refill

Terms are written as sequences of @dfn{tokens}.  Tokens are sequences of
characters which are treated as separate symbols.  Tokens include the
symbols for variables, constants and functors, as well as punctuation
characters such as brackets and commas.

We define below how lists of tokens are interpreted as terms (@pxref{Term
Token}).  Each list of tokens which is read in (for interpretation as a
term or sentence) has to be terminated by a full-stop token.  Two tokens
must be separated by a space token if they could otherwise be interpreted
as a single token.  Both space tokens and comment tokens are ignored when
interpreting the token list as a term.  A comment may appear at any point
in a token list (separated from other tokens by spaces where necessary).@refill

We define below how tokens are represented as strings of characters
(@pxref{Token String}).  But we start by describing the notation used in
the formal definition of Prolog syntax (@pxref{Syntax Notation}).
@refill

@subsection Notation
@cindex syntax notation

@enumerate

@item
Syntactic categories (or @dfn{non-terminals}) are written thus:
@var{item}.  Depending on the section, a category may represent a class
of either terms, token lists, or character strings. @refill

@item
A syntactic rule takes the general form 

@example
@var{C} --> @var{F1} | @var{F2} | @var{F3}
@end example

@noindent
which states that an entity of category @var{C} may take any of the alternative
forms @var{F1}, @var{F2}, @var{F3}, etc. @refill

@item
Certain definitions and restrictions are given in ordinary English,
enclosed in @{ @} brackets. @refill

@item
A category written as @var{C...} denotes a sequence of one or more
@var{C}s. @refill

@item
A category written as @var{?C} denotes an optional @var{C}.
Therefore @var{?C...} denotes a sequence of zero or more
@var{C}s.@refill

@item
A few syntactic categories have names with arguments, and rules in which
they appear may contain meta-variables looking thus: @var{X}.  The meaning
of such rules should be clear from analogy with the definite clause
grammars (@pxref{Definite}). @refill

@item
In the section describing the syntax of terms and tokens (@pxref{Term
Token}) particular tokens of the category name are written thus:
@var{name}, while tokens which are individual punctuation characters are
written literally. @refill
@end enumerate

@node Sentence, Term Token, Syntax Notation, Full Syntax
@comment  node-name,  next,  previous,  up
@subsection Syntax of Sentences as Terms
@cindex syntax of sentences
@cindex sentence

@example
@var{sentence}          --> @var{clause} | @var{directive} | @var{grammar-rule}

@var{clause}            --> @var{non-unit-clause} | @var{unit-clause}

@var{directive}         --> @var{command} | @var{query}

@var{non-unit-clause}   --> @var{head} @kbd{:-} @var{goals}

@var{unit-clause}       --> @var{head}
                         @r{@{ where @var{head} is not otherwise a @var{sentence} @}}

@var{command}           --> @kbd{:-} @var{goals}

@var{query}             --> @kbd{?-} @var{goals}

@var{head}              --> term
                         @r{@{ where term is not a @var{number} or @var{variable} @}}

@var{goals}             --> @var{goals} @kbd{,} @var{goals}
                   |  @var{goals} @kbd{->} @var{goals} @kbd{;} @var{goals}
                   |  @var{goals} @kbd{->} @var{goals}
                   |  @kbd{\+} @var{goals}
                   |  @var{goals} @kbd{;} @var{goals}
                   |  @var{goal}

@var{goal}              --> term
                         @r{@{ where term is not a @var{number}
                           and is not otherwise a @var{goals} @}}

@var{grammar-rule}      --> @var{gr-head} @kbd{-->} @var{gr-body}

@var{gr-head}           --> @var{non-terminal}
                   |  @var{non-terminal} @kbd{,} @var{terminals}

@var{gr-body}           --> @var{gr-body} @kbd{,} @var{gr-body}
                   |  @var{gr-body} @kbd{->} @var{gr-body} @kbd{;} @var{gr-body}
                   |  @var{gr-body} @kbd{->} @var{gr-body}
                   |  @kbd{\+} @var{gr-body}
                   |  @var{gr-body} @kbd{;} @var{gr-body}
                   |  @var{non-terminal}
                   |  @var{terminals}
                   |  @var{gr-condition}

@var{non-terminal}      --> term
                         @r{@{ where term is not a @var{number} or @var{variable}
                           and is not otherwise a @var{gr-body} @}}

@var{terminals}         --> list | string

@var{gr-condition}      --> @kbd{@{} @var{goals} @kbd{@}}
@end example

@node Term Token, Token String, Sentence, Full Syntax
@comment  node-name,  next,  previous,  up
@subsection Syntax of Terms as Tokens
@cindex syntax of terms

@example
@var{term-read-in}      --> @var{subterm(1200)} @var{full-stop}

@var{subterm(N)}        --> @var{term(M)}
                         @r{@{ where @var{M} is less than or equal to @var{N} @}}

@var{term(N)}           --> @var{op(N,fx)} @var{subterm(N-1)}
                         @r{@{ except the case @kbd{-} @var{number} @}}
                         @r{@{ if subterm starts with a @kbd{(},
                           @var{op} must be followed by a @var{space} @}}
                   |  @var{op(N,fy)} @var{subterm(N)}
                         @r{@{ if subterm starts with a @kbd{(},}
                           @r{op must be followed by a @var{space} @}}
                   |  @var{subterm(N-1)} @var{op(N,xfx)} @var{subterm(N-1)}
                   |  @var{subterm(N-1)} @var{op(N,xfy)} @var{subterm(N)}
                   |  @var{subterm(N)} @var{op(N,yfx)} @var{subterm(N-1)}
                   |  @var{subterm(N-1)} @var{op(N,xf)}
                   |  @var{subterm(N)} @var{op(N,yf)}

@var{term(1000)}        --> @var{subterm(999)} @kbd{,} @var{subterm(1000)}

@var{term(0)}           --> @var{functor} @kbd{(} @var{arguments} @kbd{)}
                         @r{@{ provided there is no space between}
                           @r{the @var{functor} and the @kbd{(} @}}
                   |  @kbd{(} @var{subterm(1200)} @kbd{)}
                   |  @kbd{@{} @var{subterm(1200)} @kbd{@}}
                   |  @var{list}
                   |  @var{string}
                   |  @var{constant}
                   |  @var{variable}

@var{op(N,T)}           --> @var{name}
                         @r{@{ where @var{name} has been declared as an}
                           @r{operator of type @var{T} and precedence @var{N} @}}

@var{arguments}         --> @var{subterm(999)}
                   |  @var{subterm(999)} @kbd{,} @var{arguments}

@var{list}              --> @kbd{[]}
                   |  @kbd{[} @var{listexpr} @kbd{]}

@var{listexpr}          --> @var{subterm(999)}
                   |  @var{subterm(999)} @kbd{,} @var{listexpr}
                   |  @var{subterm(999)} @kbd{|} @var{subterm(999)}

@var{constant}          --> @var{atom} | @var{number}

@var{number}            --> @var{integer} | @var{float}

@var{atom}              --> @var{name}

@var{integer}           --> @var{natural-number}
                   |  @kbd{-} @var{natural-number}

@var{float}             --> @var{unsigned-float}
                   |  @kbd{-} @var{unsigned-float}

@var{functor}           --> @var{name}
@end example

@node Token String, Syntax Notes, Term Token, Full Syntax
@comment  node-name,  next,  previous,  up
@subsection Syntax of Tokens as Character Strings
@cindex syntax of tokens

By default, SICStus uses the ISO 8859/1 character set standard, but will
alternatively support the EUC (Extended UNIX Code) standard.  This is
governed by the value of the environment variable @code{LC_CTYPE}
(@pxref{Installation Intro}).

The character categories used below are defined as follows in the two
standards:

@table @var

@item layout-char
In ISO 8859/1, these are ASCII codes 0..32 and 127..159.  In EUC, these
are ASCII codes 0..32 and 127.  The common subset includes characters
such as @key{TAB}, @key{LFD}, and @key{SPC}.

@item small-letter
In ISO 8859/1, these are ASCII codes 97..122, 223..246, and 248..255.
In EUC, these are ASCII codes 97..122 and 128..255.  The common subset
are the letters @kbd{a} through @kbd{z}.

@item capital-letter
In ISO 8859/1, these are ASCII codes 65..90, 192..214, and 216..222.  In
EUC, these are ASCII codes 65..90.  The common subset are the letters
@kbd{A} through @kbd{Z}.

@item digit
In both standards, these are ASCII codes 48..57, i.e. the digits
@kbd{0} through @kbd{9}.

@item symbol-char
In ISO 8859/1, these are ASCII codes 35, 36, 38, 42, 43, 45..47, 58,
60..64, 92, 94, 96, 126, 160..191, 215, and 247.  In EUC, these are
ASCII codes 35, 36, 38, 42, 43, 45..47, 58, 60..64, 92, 94, 96, and 126.
The common subset is@*
@kbd{+}@kbd{-}@kbd{*}@kbd{/}@kbd{\}@kbd{^}@kbd{<}@kbd{>}@kbd{=}@kbd{`}@kbd{~}@kbd{:}@kbd{.}@kbd{?}@kbd{@@}@kbd{#}@kbd{$}@kbd{&}.

@item solo-char
In both standards, these are ASCII codes 33 and 59 i.e. the characters
@kbd{!} and @kbd{;}.

@item punctuation-char
In both standards, these are ASCII codes 37, 40, 41, 44, 91, 93, and
123..125, i.e. the characters @kbd{%(),[]@{|@}}.

@item quote-char
In both standards, these are ASCII codes 34 and 39 i.e. the characters
@kbd{"} and @kbd{'}.

@item underline
In both standards, this is ASCII code 95 i.e. the character @kbd{_}.
@end table

@example
@var{token}             --> @var{name}
                   |  @var{natural-number}
                   |  @var{unsigned-float}
                   |  @var{variable}
                   |  @var{string}
                   |  @var{punctuation-char}
                   |  @var{space}
                   |  @var{comment}
                   |  @var{full-stop}

@var{name}              --> @var{quoted-name}
                   |  @var{word}
                   |  @var{symbol}
                   |  @var{solo-char}
                   |  @kbd{[} @var{?layout-char...} @kbd{]}
                   |  @kbd{@{} @var{?layout-char...} @kbd{@}}

@var{quoted-name}       --> @kbd{'} @var{?quoted-item...} @kbd{'}

@var{quoted-item}       --> @var{char}  @r{@{ other than @kbd{'} @}}
                   |  @kbd{''}

@var{word}              --> @var{small-letter} @var{?alpha...}

@var{symbol}            --> @var{symbol-char...}
                         @r{@{ except in the case of a @var{full-stop}}
                           @r{or where the first 2 chars are @kbd{/*} @}}

@var{natural-number}    --> @var{digit...}
                   |  @var{base} @kbd{'} @var{alpha...}
                      @r{@{ where each @var{alpha} must be less than the @var{base},}
                         @r{treating a,b,... and A,B,... as 10,11,... @}}
                   |  @kbd{0} @kbd{'} @var{char}
                      @r{@{ yielding the ASCII code for @var{char} @}}
  
@var{base}              --> @var{digit...}  @r{@{ in the range [2..36] @}}

@var{unsigned-float}    --> @var{simple-float}
                   |  @var{simple-float} @var{exp} @var{exponent}

@var{simple-float}      --> @var{digit...} @kbd{.} @var{digit...}

@var{exp}               --> @kbd{e}  |  @kbd{E}

@var{exponent}          --> @var{digit...} | @kbd{-} @var{digit...} | @kbd{+} @var{digit...}

@var{variable}          --> @var{underline} @var{?alpha...}
                   |  @var{capital-letter} @var{?alpha...}

@var{string}            --> @kbd{"} @var{?string-item...} @kbd{"}

@var{string-item}       --> @var{char}  @r{@{ other than @kbd{"} @}}
                   |  @kbd{""}

@var{space}             --> @var{layout-char...}

@var{comment}           --> @kbd{/*} @var{?char...} @kbd{*/}
                         @r{@{ where @var{?char...} must not contain @kbd{*/} @}}
                   |  @kbd{%} @var{?not-end-of-line...} @var{newline}

@var{not-end-of-line}   --> @r{@{ any character except @var{newline} @}}

@var{newline}           --> @r{@{ @key{LFD} @}}

@var{full-stop}         --> @kbd{.} @var{layout-char}

@var{char}              --> @r{@{ any ASCII character, i.e. @}}
                      @var{layout-char}
                   |  @var{alpha}
                   |  @var{symbol-char}
                   |  @var{solo-char}
                   |  @var{punctuation-char}
                   |  @var{quote-char}

@var{alpha}             --> @var{capital-letter} | @var{small-letter} | @var{digit} | @var{underline}
@end example

@node Syntax Notes,  , Token String, Full Syntax
@comment  node-name,  next,  previous,  up
@subsection Notes

@enumerate

@item
The expression of precedence 1000 (i.e. belonging to syntactic category
@var{term(1000)}) which is written

@example
@var{X},@var{Y}
@end example

@noindent
denotes the term @code{','(@var{X},@var{Y})} in standard syntax.

@item
The parenthesised expression (belonging to syntactic category
@var{term(0)}) @refill

@example
(@var{X})
@end example

@noindent
denotes simply the term @code{@var{X}}.

@item
The curly-bracketed expression (belonging to syntactic category
@var{term(0)}) @refill

@example
@{@var{X}@}
@end example

@noindent
denotes the term @code{@{@}(@var{X})} in standard syntax.

@item
Note that, for example, @code{-3} denotes a number whereas @code{-(3)}
denotes a compound term which has the 1-ary functor @code{-} as its principal
functor. @refill

@item
The character @kbd{"} within a string must be written duplicated.
Similarly for the character @kbd{'} within a quoted atom. @refill

@item
A name token declared to be a prefix operator will be treated as an atom
only if no @var{term-read-in} can be read by treating it as a prefix
operator.

@item
A name token declared to be both an infix and a postfix operator will be
treated as a postfix operator only if no @var{term-read-in} can be read
by treating it as an infix operator.
@end enumerate



").

main.
