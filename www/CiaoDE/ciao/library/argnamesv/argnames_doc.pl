:- use_package(assertions).
:- comment(nodoc,assertions).

:- comment(title,"Terms with named arguments -records/feature terms").

:- comment(bug,"It would be nice to add a mechanism to portray terms
    with named arguments in a special (user definable) way.").

:- comment(author,"Daniel Cabeza and Manuel Hermenegildo").

:- comment(module, "@cindex{Naming term aguments} This library package
   provides syntax which allows accessing term arguments by name
   (these terms are sometimes also referred to as @index{records}, and
   are also similar to @index{feature terms}
   @cite{Ait-KaciPodelskiSmolka92}).").

:- include(library(argnames)).

:- decl argnames(ArgNamedPredSpec) 

# "An @decl{argnames/1} declaration assigns names to the argument
   positions of terms (or literal/goals) which use a certain
   functor/arity. This allows referring to these arguments by their
   name rather than by their argument position. Sometimes, argument
   names may be clearer and easier to remember than argument
   positions, specially for predicates with many arguments. Also, in
   some cases this may allow adding arguments to certain predicates
   without having to change the code that uses them.  These terms with
   named arguments are sometimes also referred to as
   @concept{records}, and are also similar to @concept{feature terms}
   @cite{Ait-KaciPodelskiSmolka92}. For example, in order to write a
   program for the @em{zebra} puzzle we might declare:

@noindent
@begin{verbatim}
:- use_package([argnames]).
:- argnames house(color, nation, pet, drink, car).
@end{verbatim}

   @noindent which first includes the package and then assigns a name
   to each of the arguments of any term (or literal/goal) with
   @tt{house/5} as the main functor.

   Once an @decl{argnames/1} is given, is possible to use the names to
   refer to the arguments of any term (or literal/goal) which has the
   same main functor as that of the term which appears in the
   @decl{argnames/1} declaration. This is done by first writing the
   functor name, then the infix operator @tt{$}, and then, between
   curly brackets, zero, one, or more pairs
   @em{argument-name}@tt{=>}@em{argument-value}, separated by commas
   (i.e., the infix operator @tt{=>} is used between the name and the
   value). Again, argument names must be atomic. Argument values can
   be any term.  Arguments which are not specified are assumed to have
   a value of ``@tt{_}'' (i.e., they are left unconstrained). 

   Thus, after the declaration for @tt{house/5} in the example above,
   any ocurrence in that code of, for example,
   @tt{house$@{nation=>Owns_zebra,pet=>zebra@}} is exactly equivalent
   to @tt{house(_,Owns_zebra,zebra,_,_)}. Also, @tt{house$@{@}} is
   equivalent to @tt{house(_,_,_,_,_)}. The actual zebra puzzle
   specification might include a clause such as:

@noindent
@begin{verbatim}
zebra(Owns_zebra, Drinks_water, Street) :-
   Street = [house$@{@},house$@{@},house$@{@},house$@{@},house$@{@}],
   member(house$@{nation=>Owns_zebra,pet=>zebra@}, Street),
   member(house$@{nation=>Drinks_water,drink=>water@}, Street),
   member(house$@{drink=>coffee,color=>green@}, Street),
   left_right(house$@{color=>ivory@}, house$@{color=>green@}, Street),
   member(house$@{car=>porsche,pet=>snails@}, Street),
        ...
@end{verbatim}

   Any number of @decl{argnames/1} declarations can appear in a file,
   one for each functor whose arguments are to be accessed by name.
   As with other packages, argument name declarations are @em{local to
   the file} in which they appear. The @decl{argnames/1} declarations
   affect only program text which appears after the
   declaration. It is easy to make a set of declarations affect
   several files for example by putting such declarations in a
   sepatate file which is included by all such files.

   An @decl{argnames/1} declaration does not change in any way the
   internal representation of the associated terms and does not affect
   run-time efficiency. It is simply syntactic sugar.

".

:- comment(appendix,"

   Two simple examples of the use of the argnames library package
   follow.  

    @subsection{Using argument names in a toy database}

@noindent
@begin{verbatim}
@includeverbatim{examples/simple_db}
@end{verbatim}

    @subsection{Complete code for the zebra example}

@noindent
@begin{verbatim}
@includeverbatim{examples/zebra_argnames}
@end{verbatim}

   ").
