
:- use_package([assertions,regtypes]).
:- comment(nodoc,assertions).
:- comment(nodoc,regtypes).

:- comment(title,"File caching for facts").
:- comment(author,"Francisco Bueno").
:- comment(module,"This package allows to use files as a ``@concept{cache}''
   for predicates defined by facts. This is useful for huge tables of
   facts that may push the memory limits of the system too far. Goals of a
   cached predicate are 
   executed simply by reading from the corresponding file. Anything in the file
   different from a fact for the corresponding predicate is ignored. Each call
   to a predicate cached in this way forces opening the file, so the use
   of this package is subject to the limit on the number of open files
   that the system can support.").

:- comment(bug,"Does not care of module expansion, so it may not work
	properly with imported predicates.").
:- comment(bug,"It leaves cache files opened, unless all backtracking on
	the corresponding goals of the cached predicates is exhausted.
        This may cause problems with the number of opened files (especially
        when used within ciaosh).").
:- comment(bug,"Assertions/retractions won't work properly, since the cache
	file is never loaded.").
:- comment(bug,"Should be integrated with package persdb.").

:- include(library(cache)).

:- decl cache(Spec,File) : predname * filename
	# "Predicate @var{Spec} is defined by a table of facts in 
	   @var{File}, which will act as the cache during execution.".

:- comment(hid,filename/1).
:- regtype filename(X) 
   # "@var{X} is an atom describing the name of a file.".

filename(X) :- 
	atm(X).
