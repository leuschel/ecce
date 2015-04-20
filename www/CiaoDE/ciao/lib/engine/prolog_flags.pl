:- module(prolog_flags, [
        set_prolog_flag/2, current_prolog_flag/2, prolog_flag/3,
        push_prolog_flag/2, pop_prolog_flag/1, prompt/2,
        gc/0, nogc/0, fileerrors/0, nofileerrors/0],
        [assertions, isomodes]).

:- comment(title,"Changing system behaviour and various flags").

:- comment(author,"Daniel Cabeza").
:- comment(author,"Mats Carlsson").

:- comment(usage, "These predicates are builtin in Ciao, so nothing special
   has to be done to use them.").

:- comment(module,"@cindex{prolog flag} Flags define some parameters of
   the system and control the behavior of system or library predicates.
   Each flag has a name and an associated predefined value, and except
   some system flags which are fixed in general their associated value
   is changeable.  Predefined flags in the system are:

@begin{description}

@item{@tt{version}} The Ciao version, as a term
      @tt{ciao}(@var{Version},@var{Patch}).  @var{Version} is a floating
      point number, @var{Patch} is an integer.  Unchangeable.

@item{@tt{argv}} Its value is a list of atoms representing the program
      arguments supplied when the current executable was invoked.  This
      is the value to which is instantiated the argument of the
      @pred{main/1} predicate at executable startup.  Unchangeable.

@item{@tt{bounded}} It is @tt{false}, to denote that the range of
      integers can be considered infinite (but see @pred{int/1}).
      Unchangeable.  @iso

@item{@tt{fileerrors}} If @tt{on}, predicates handling files give errors
      (throw exceptions) when a file is inexistent or an operation is
      not allowed.  If @tt{off}, fail in that conditions.  Initially
      @tt{on}.

@item{@tt{gc}} Controls whether garbage collection is done.  May be
      @tt{on} (default) or @tt{off}.

@item{@tt{gc_margin}} An integer @var{Margin}.  If less than
      @var{Margin} kilobytes are reclaimed in a garbage collection then
      the size of the garbage collected area should be increased.  Also,
      no garbage collection is attempted unless the garbage collected
      area has at least @var{Margin} kilobytes.  Initially 500.

@item{@tt{gc_trace}} Governs garbage collection trace messages.  An
      element off @tt{[on,off,terse,verbose]}. Initially @tt{off}.

@item{@tt{integer_rounding_function}} It is @tt{toward_zero}, so that
      @tt{-1 =:= -3//2} succeeds.  Unchangeable.  @iso

@item{@tt{max_arity}} It is 255, so that no compound term (or predicate)
      can have more than this number of arguments.  Unchangeable.  @iso

@item{@tt{quiet}} Controls which messages issued using @lib{io_aux} are
      actually written.  As the system uses that library to report its
      messages, this flag controls the @em{verbosity} of the system.
      Possible states of the flag are:

  @begin{description}

  @item{@tt{on}} No messages are reported.

  @item{@tt{error}} Only error messages are reported.

  @item{@tt{warning}} Only error and warning messages are reported.

  @item{@tt{off}} All messages are reported, except debug messages.
        This is the default state.

  @item{@tt{debug}} All messages, including debug messages, are
        reported.  This is only intended for the system implementators.

  @end{description}

@item{@tt{unknown}} Controls action on calls to undefined predicates.
      The possible states of the flag are:

  @begin{description}

  @item{@tt{error}} An error is thrown with the @concept{error term}
       @tt{existence_error(procedure, F/A)}.

  @item{@tt{fail}} The call simply fails.

  @item{@tt{warning}} A warning is written and the call fails.

  @end{description}

  The state is initially @tt{error}. @iso

@end{description}
  ").

:- use_module(engine(internals), [
         '$unknown'/2, '$ferror_flag'/2, '$prompt'/2, '$unix_argv'/1,
        '$quiet_flag'/2, '$gc_trace'/2, '$gc_margin'/2, '$gc_mode'/2,
        '$compiling'/2, '$ciao_version'/2 ]).

%% doinclude's below commented out because LPdoc does not allow yet a 
%% declaration and a predicate to have the same name.

%% :- comment(doinclude,set_prolog_flag/1).

:- true decl set_prolog_flag(Flag, Value) : atm * term + iso
        # "Sets the @concept{prolog flag} of name @var{Flag} to value
          @var{Value} in the rest of the current text (its scope is local).".

%% :- comment(doinclude,push_prolog_flag/1).

:- true decl push_prolog_flag(Flag, Value) : atm * term
        # "Sets the @concept{prolog flag} of name @var{Flag} to value
          @var{Value}, but storing current value of @var{Flag} to
          restore it with @decl{pop_prolog_flag/1} (its scope is local).".

%% :- comment(doinclude,pop_prolog_flag/1).

:- true decl pop_prolog_flag(Flag) : atm
        # "Restores the value of @var{Flag} previous to the last
          non-canceled declaration @decl{push_prolog_flag/2} on it.".

:- comment(define_flag(Flag, Values, Default), "New flags can be defined
   by writing facts of this predicate.  @var{Flag} is the name of the new
   flag, @var{Values} defines the posible values for the flag (see
   below) and @var{Default} defines the predefined value associated with
   the flag (which should be compatible with @var{Values}).").

:- pred define_flag(-atm,==(atom),-atm)
        # "Posible values for the flag are atoms.@p
     Example:
@begin{verbatim}
:- multifile define_flag/3.
define_flag(tmpdir, atom, '/tmp').
@end{verbatim}
".

:- pred define_flag(-atm,==(integer),-int)
        # "Posible values for the flag are integers.@p
     Example:
@begin{verbatim}
:- multifile define_flag/3.
define_flag(max_connections, integer, 10).
@end{verbatim}
".

:- pred define_flag(atm,list(Values),Default) =>
        member(Default, Values)
        # "Posible values for the flag are the elements of @var{Values}.@p
     Example:
@begin{verbatim}
:- multifile define_flag/3.
define_flag(debug, [on,debug,trace,off], off).
@end{verbatim}
".

:- multifile define_flag/3.

:- comment(set_prolog_flag(FlagName,Value),
           "Set existing flag @var{FlagName} to @var{Value}.").
:- true pred set_prolog_flag(+atm,+term) + iso.

set_prolog_flag(X, Y) :- nonvar(X), prolog_flag(X, _, Y), !. /* ISO */

:- comment(current_prolog_flag(FlagName,Value),
           "@var{FlagName} is an existing flag and @var{Value} is the
           value currently associated with it.").

:- true pred current_prolog_flag(?atm,?term) + iso.

current_prolog_flag(X, Y) :- prolog_flag(X, Y, Y). /* ISO */

:- comment(prolog_flag(FlagName,OldValue,NewValue), "@var{FlagName} is
           an existing flag, unify @var{OldValue} with the value
           associated with it, and set it to new value @var{NewValue}.").

:- true pred prolog_flag(?atm,?term,+term).

:- true pred prolog_flag(?FlagName,-OldValue,-NewValue)
        : (atm(FlagName), OldValue == NewValue)
        # "Same as @tt{current_prolog_flag(@var{FlagName}, @var{OldValue})}".

prolog_flag(Flag, Old, New) :- var(Flag), !,
	prolog_flag_2(Flag, Old, New).
prolog_flag(Flag, Old, New) :-
	prolog_flag_2(Flag, Old, New), !.

prolog_flag_2(compiling, Old, New) :-
	flag_value(Old, New, [unprofiled,profiled]),
	'$compiling'(Old, New).
prolog_flag_2(fileerrors, Old, New) :-
	flag_value(Old, New, [on,off]),
	'$ferror_flag'(Old, New).
prolog_flag_2(gc, Old, New) :-
	flag_value(Old, New, [on,off]),
	'$gc_mode'(Old, New).
prolog_flag_2(gc_margin, Old, New) :-
	flag_value(Old, New, integer),
	'$gc_margin'(Old, New).
prolog_flag_2(gc_trace, Old, New) :-
	flag_value(Old, New, [on,off,terse,verbose]),
	'$gc_trace'(Old, New).
prolog_flag_2(unknown, Old, New) :-
	flag_value(Old, New, [error,fail,warning]),
	'$unknown'(Old, New).
prolog_flag_2(quiet, Old, New) :-
        flag_value(Old, New, [on,error,warning,debug,off]),
        '$quiet_flag'(Old, New).
prolog_flag_2(version, Version_Term, Version_Term) :-
        '$ciao_version'(Version, Patch),
        Version_Term = ciao(Version, Patch).
prolog_flag_2(argv, Args, Args) :-
        '$unix_argv'(Args).
prolog_flag_2(bounded, false, false). % ISO 
prolog_flag_2(integer_rounding_function, toward_zero, toward_zero). % ISO
prolog_flag_2(max_arity, 255, 255). % ISO
prolog_flag_2(Flag, Old, New) :-
        define_flag(Flag, Values, Default),
        flag_value(Old, New, Values),
        set_flag(Flag, Default, Old, New).

flag_value(Old, New, _) :- var(New), !, Old==New.
flag_value(_, New, Xs) :- flag_value_check(Xs, New).

flag_value_check(atom, X) :- atom(X).
flag_value_check(integer, X) :- integer(X).
flag_value_check([X|_], X) :- !.
flag_value_check([_|Xs], X) :- flag_value_check(Xs, X).

:- data flag/2.

set_flag(Flag, Default, Old, New) :-
	( current_fact(flag(Flag,Tmp),Ptr)
	-> Tmp=Old
        ; asserta_fact(flag(Flag,Default),Ptr),
          Default=Old ),
	( Old==New
	-> true
        ; erase(Ptr),
          asserta_fact(flag(Flag,New)) ).

:- data old_flag/2.

:- comment(push_prolog_flag(Flag, NewValue), "Same as
   @pred{set_prolog_flag/2}, but storing current value of @var{Flag} to
   restore it with @pred{pop_prolog_flag/1}.").

:- true pred push_prolog_flag(+atm, +term).

push_prolog_flag(Flag, NewValue) :-
        nonvar(Flag),
        prolog_flag(Flag, OldValue, NewValue),
        asserta_fact(old_flag(Flag, OldValue)).

:- comment(pop_prolog_flag(Flag), "Restore the value of @var{Flag}
   previous to the last non-canceled @pred{push_prolog_flag/2} on it.").

:- true pred pop_prolog_flag(+atm).

pop_prolog_flag(Flag) :-
        nonvar(Flag),
        retract_fact(old_flag(Flag, OldValue)),
        prolog_flag(Flag, _, OldValue).

:- comment(prompt(Old, New), "Unify @var{Old} with the current prompt
   for reading, change it to @var{New}.").

:- true pred prompt(?atm,+atm).

:- true pred prompt(Old,New) : (var(Old), var(New), Old == New)
                            => (atm(Old), atm(New))
        # "Unify @var{Old} with the current prompt for reading without
          changing it.".

prompt(Old, New) :-
	flag_value(Old, New, atom),
	'$prompt'(Old, New).

:- pred fileerrors/0 # "Enable reporting of file errors.  Equivalent to
        @tt{set_prolog_flag(fileerrors, on)}".

fileerrors :- '$ferror_flag'(_, on).

:- pred nofileerrors/0 # "Disable reporting of file errors.  Equivalent
        to @tt{set_prolog_flag(fileerrors, off)}".

nofileerrors :- '$ferror_flag'(_, off).

:- pred gc/0 # "Enable garbage collection.  Equivalent to
        @tt{set_prolog_flag(gc, on)}".

gc :- '$gc_mode'(_, on).

:- pred nogc/0 # "Disable garbage collection.  Equivalent to
        @tt{set_prolog_flag(gc, off)}".

nogc :- '$gc_mode'(_, off).
