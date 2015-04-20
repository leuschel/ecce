:- module(prolog_sys, [
        statistics/0, statistics/2, predicate_property/2,
        current_atom/1, garbage_collect/0,
        new_atom/1],
        [assertions, isomodes]). 

:- impl_defined([
        statistics/0,
        current_atom/1,
        new_atom/1,
        garbage_collect/0]).

:- use_module(engine(internals)).

:- comment(title, "Prolog system internal predicates").
:- comment(author, "Manuel Carro").
:- comment(author, "Daniel Cabeza").
:- comment(author, "Mats Carlsson").

:- comment(module, "This module implements some miscellaneous predicates
   which provide access to some internal statistics, special properties
   of the predicates, etc.").

:- comment(bug, "The space used by the process is not measured here:
process data, code, and stack also take up memory.  The memory
reported for atoms is not what is actually used, but the space used up
by the hash table (which is enlarged as needed).").

:- true pred statistics # "Prints statistics about the system.".

:- true pred statistics(Click_option, Click_result) : click_option *
term => click_option * click_result # "Gather information about clock
clicks (either run, user, system or wall click) since last consult or
since start of program.  A click is the smallest amount of time that a
clock can measure.".

:- true pred statistics(Clockfreq_option, Clockfreq_result) :
clockfreq_option * term => clockfreq_option * clockfreq_result #
"Gather information about frequency of the clocks used to measure the
clicks (either run-user, system or wall clock).  Results are returned
in hertzios.  This value also can be defined as the amount of clicks
that a clock can measure in one second.".

:- true pred statistics(Time_option, Time_result) : time_option * term
=> time_option * time_result # "Gather information about time (either
process time or wall time) since last consult or since start of
program.  Results are returned in milliseconds. Note that internally,
time is calculated as:

@begin{verbatim}
  Time_result = (Click_result / Clockfreq_result) * 1000
@end{verbatim}

".

:- true pred statistics(Memory_option, Memory_result) : memory_option * term =>
memory_option * memory_result # "Gather information about memory
consumption.".

:- true pred statistics(Garbage_collection_option, Gc_result) : garbage_collection_option *
term => garbage_collection_option * gc_result # "Gather information
about garbage collection.".

:- true pred statistics(Symbol_option, Symbol_result) :symbol_option * term =>
symbol_option * symbol_result # "Gather information about number of
symbols and predicates.".

:- true pred statistics(Option, ?term) # "If @var{Option} is unbound,
it is bound to the values on the other cases.".

:- true pred garbage_collect # "Forces garbage collection when called.".

:- true pred current_atom(Atom) : var => atm # "Enumerates on
backtracking all the existing atoms in the system.".

:- true pred new_atom(Atom) : var => atm # "Returns, on success, a new
atom, not existing before in the system.  The entry argument must be a
variable.  The idea behind this atom generation is to provide a fast
source of identifiers for new objects, concurrent predicates, etc. on
the fly.".

:- comment(doinclude, time_option/1).  

:- true prop time_option(M) + regtype # "Options to get information
about execution time.  @var{M} must be one of @tt{runtime},
@tt{usertime}, @tt{systemtime} or @tt{walltime}.".

time_option(runtime).
time_option(usertime).
time_option(systemtime).
time_option(walltime).

:- true prop click_option(M) + regtype # "Options to get information about
   execution clicks.".

click_option(runclick).
click_option(userclick).
click_option(systemclick).
click_option(wallclick).  

:- true prop clockfreq_option(M) + regtype # "Options to get information about
   the frequency of clocks used to get the clicks.".

clockfreq_option(userclockfreq).
clockfreq_option(systemclockfreq).
clockfreq_option(wallclockfreq).

:- comment(doinclude, memory_option/1).

:- true prop memory_option(M) + regtype # "Options to get information about
memory usage.".

memory_option(memory).
memory_option(symbols).
memory_option(program).
memory_option(global_stack).
memory_option(local_stack).
memory_option(trail).
memory_option(choice).


:- comment(doinclude, garbage_collection_option/1).

:- true prop garbage_collection_option(M) + regtype # "Options to get
   information about garbage collection.".

garbage_collection_option(garbage_collection).
garbage_collection_option(stack_shifts).


:- comment(doinclude, symbol_option/1).

:- true prop symbol_option(M) + regtype # "Option to get information
   about the number of symbols in the program.".

symbol_option(symbols).


:- comment(doinclude, time_result/1).

:- true prop time_result(Result) + regtype # "@var{Result} is a
two-element list of numbers.  The first number is the time since the
start of the execution; the second number is the time since the
previous consult to time.".

time_result([A, B]):- num(A), num(B).

:- comment(doinclude, click_result/1).

:- true prop click_result(Result) + regtype # "@var{Result} is a
two-element list of numbers.  The first number is the number of clicks
since the start of the execution; the second number is the number of
clicks since the previous consult to click.".

click_result([A, B]):- num(A), num(B).

:- comment(doinclude, clockfreq_result/1).

:- true prop clockfreq_result(Result) + regtype # "@var{Result} is a
number.  It gives the frequency in hertzios used by the clock used to
get the clicks.".

clockfreq_result(A):- num(A).

:- comment(doinclude, memory_result/1).

:- true prop memory_result(Result) + regtype # "Result is a
two-element list of integers.  The first element is the space taken up
by the option selected, measured in bytes; the second integer is zero
for program space (which grows as necessary), and the amount of free
space otherwise.".

memory_result([A, B]):- int(A), int(B).


:- comment(doinclude, gc_result/1).

:- true prop gc_result(Result) + regtype # "@var{Result} is a
tree-element list of integers, related to @concept{garbage collection}
and @concept{memory management}.  When @tt{stack_shifts} is selected,
the first one is the number of shifts (reallocations) of the local
stack; the second is the number of shifts of the trail, and the third
is the time spent in these shifts.  When @tt{garbage_collection} is
selected, the numbers are, respectively, the number of garbage
collections performed, the number of bytes freed, and the time spent
in garbage collection.".

gc_result([A, B, C]):- int(A), int(B), int(C).


:- comment(doinclude, symbol_result/1).

:- true prop symbol_result(Result) + regtype # "@var{Result} is a
   two-element list of integers.  The first one is the number of atom,
   functor, and predicate names in the symbol table.  The second is
   the number of predicates known to be defined (although maybe
   without clauses).".

symbol_result([A, B]):- int(A), int(B).

 %% memory_option(core).
 %% memory_option(heap).

statistics(runtime, L) :- '$runtime'(L).
statistics(usertime, L) :- '$usertime'(L).
statistics(systemtime, L) :- '$systemtime'(L).
statistics(walltime, L) :- '$walltime'(L).

statistics(runclick, L) :- '$runclick'(L).
statistics(userclick, L) :- '$userclick'(L).
statistics(systemclick, L) :- '$systemclick'(L).
statistics(wallclick, L) :- '$wallclick'(L).

statistics(userclockfreq, L) :- '$userclockfreq'(L).
statistics(systemclockfreq, L) :- '$systemclockfreq'(L).
statistics(wallclockfreq, L) :- '$wallclockfreq'(L).

statistics(memory, L) :- '$total_usage'(L).
statistics(symbols, L) :- '$internal_symbol_usage'(L).
statistics(program, L) :- '$program_usage'(L).
statistics(global_stack, L) :- '$termheap_usage'(L).
statistics(local_stack, L) :- '$envstack_usage'(L).
statistics(trail, L) :- '$trail_usage'(L).
statistics(choice, L) :- '$choice_usage'(L).
statistics(core, L) :- statistics(memory, L).
statistics(heap, L) :- statistics(program, L).

statistics(garbage_collection, L) :- '$gc_usage'(L).
statistics(stack_shifts, L) :- '$stack_shift_usage'(L).

:- true pred predicate_property(Head, Property)
   : term * term => callable * atm
   # "The predicate with clause @var{Head} is @var{Property}.".

% :- primitive_meta_predicate(predicate_property(fact,?)).

predicate_property(Head, Prop) :-
	'$predicate_property'(Head, Entry, Bits), % xref nondet.c
	(   Entry=8 -> BaseProp=interpreted	% xref predtyp.h
        ;   BaseProp=compiled
        ),
	predicate_property_(Bits, BaseProp, Prop).

predicate_property_(0, P, P) :- !.
predicate_property_(_, P, P).
predicate_property_(Bits0, _, P) :-
	Bits is Bits0/\(Bits0-1),
	B is Bits0-Bits,
	bit_decl(B, Prop),
	predicate_property_(Bits, Prop, P).

bit_decl(1, (concurrent)).
bit_decl(2, (dynamic)).
bit_decl(4, (wait)).
bit_decl(8, (multifile)).
