:- use_package(assertions).
:- comment(nodoc,assertions).

:- comment(title,"The Ciao Profiler").
:- comment(subtitle,"REFERENCE MANUAL").
:- comment(subtitle,"@em{Generated/Printed on:} @today{}").

:- comment(author,"Edison Mera").
:- comment(copyright,"Copyright @copyright{} M. Hermenegildo

@include{Copyright.Manuals}

").

:- comment(usage, "The ciao profiler is used by including
   @tt{profiler} in the include list of a module, or by means of an
   explicit @tt{:- use_package(profiler)}.").

:- comment(summary, "The profiler module provides predicates and
   declarations for handle automatically profiling of the program to
   measure the time or cumulative statistics such as number of calls
   of predicates.  This const of two parts one in high level prolog
   language, and other implemented in C, that requires the engine must
   have compiled with the option DEBUG_LEVEL=-profile.  For more
   information, see the SETTINGS.pl file.").

:- comment(module, "The ciao profiler provides a high-level, flexible
way to mark a predicate for profiling, based on the use of assertions
to indicate if a predicate must be instrumented or not.

By default, if the user don't indicates that any predicate is going to
be instrumented, all the predicates in the given module are
instrumented.

The use of at least one assertion saying that an specific predicate
must be instrumented overrides the behavior above.  The declaration is
as follows:

@begin{verbatim}
	:- profile pred1/Arity1, ... predN/AritiyN.
@end{verbatim}

where @pred{pred1/Arity1}, ..., @pred{predN/ArityN} are the predicates
to be instrumented.

Other useful declaration let us to indicate that one predicate must
not be instrumented, and is declared as:

@begin{verbatim}
	:- noprofile pred1/Arity1, ... predN/AritiyN.
@end{verbatim}

where @pred{pred1/Arity1}, ..., @pred{predN/ArityN} are the predicates
that will not be instrumented.

").
