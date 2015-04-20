:- use_package(assertions).
:- comment(nodoc,assertions).

:- comment(title, "Breadth-first execution").

:- comment(author, "Daniel Cabeza").
:- comment(author, "Manuel Carro").

:- comment(module,"This package implements breadth-first execution of
   predicates.  Predicates written with operators @op{'<-'/1} (facts) and
   @op{'<-'/2} (clauses) are executed using breadth-first search.  This may
   be useful in search problems when a @concept{complete proof procedure}
   is needed.  An example of code would be:
@begin{verbatim}
@includeverbatim{examples/chain.pl}
@end{verbatim}

   There is another version, called @lib{bf/af}, which ensures
   AND-fairness by goal shuffling.  This version correctly says
   ``@tt{no}'' executing the following test:
@begin{verbatim}
@includeverbatim{examples/sublistapp.pl}
@end{verbatim}
").

:- comment(bug, "Does not correctly work in user files.").

:- include(library('bf/ops')).
