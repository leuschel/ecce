
/* ----------------------------------------- */
/* (C) COPYRIGHT MICHAEL LEUSCHEL 1995,96,97 */
/* ----------------------------------------- */


/* ========= */
/* DEBUGGING */
/* ========= */

:- dynamic debug_printing/1.



/* ---------------- */
/* debug_printing/1 */
/* ---------------- */

debug_printing(off). /* on  or  off */

/* ------------------------------------ */
/* set_debug_printing_value/0 */
/* ------------------------------------ */

%set_debug_printing_value :-
%	print('Debugging Information Printing:'),nl,
%	print('on: (Print Debugging Info)'),nl,
%	print('off:(No Debugging info)'),nl,
%	print('Current choice: '),
%	debug_printing(Cur),
%	print(Cur),nl,
%	print('choice =>'),
%	read(NewValue),
%	((not(NewValue=on),not(NewValue=off))
%	 -> (print('Illegal value, assuming off'),nl,
%	     set_debug_printing(off))
%	 ;  (set_debug_printing(NewValue))
%	).

set_debug_printing_value(Val) :-
	(exec_mode(interactive) 
	->(print('Debugging Information Printing:'),nl,
	   print('on: (Print Debugging Info)'),nl,
	   print('off:(No Debugging info)'),nl,
	   print('Current choice: '),
	   debug_printing(Cur),
	   print(Cur),nl,
	   print('choice =>'),read(NewValue))
	; NewValue = Val),	
	((not(NewValue=on),not(NewValue=off))
	 -> (print('Illegal value, assuming off'),nl,
	     set_debug_printing(off))
	 ;  (set_debug_printing(NewValue))
	).

/* -------------------- */
/* set_debug_printing/1 */
/* -------------------- */

set_debug_printing(_NewVal) :-
	retract(debug_printing(_Cur)),
	fail.
set_debug_printing(NewVal) :-
	asserta(debug_printing(NewVal)).




/* ------------- */
/* debug_print/1 */
/* ------------- */

debug_print(X) :-
	debug_printing(on),!,
	print(X).
debug_print(_X).

/* --------- */
/* debug_nl/ */
/* --------- */

debug_nl :-
	debug_printing(on),!,
	nl.
debug_nl.


debug_check_point(X) :-
	debug_printing(on),!,
	print(X),nl,
	print('### => Debugging Check Point:  => '),
       read(_Answer).
debug_check_point(_X).

/* ========= */
/*  TRACING  */
/* ========= */

:- dynamic trace_printing/1.



/* ---------------- */
/* trace_printing/1 */
/* ---------------- */

trace_printing(off). /* on  or  off */

/* ------------------------------------ */
/* set_trace_printing_value/0 */
/* ------------------------------------ */

set_trace_printing_value(Val) :-
	(exec_mode(interactive) 
	->(print('Tracing Information Printing:'),nl,
	   print('on: (Print Tracing Info)'),nl,
	   print('off:(No Tracing info)'),nl,
	   print('Current choice: '),
	   trace_printing(Cur),
	   print(Cur),nl,
	   print('choice =>'),read(NewValue))
	; NewValue = Val),	
	((not(NewValue=on),not(NewValue=off))
	 -> (print('Illegal value, assuming off'),nl,
	     set_trace_printing(off))
	 ;  (set_trace_printing(NewValue))
	).

/* -------------------- */
/* set_trace_printing/1 */
/* -------------------- */

set_trace_printing(_NewVal) :-
	retract(trace_printing(_Cur)),
	fail.
set_trace_printing(NewVal) :-
	asserta(trace_printing(NewVal)).




/* ------------- */
/* trace_print/1 */
/* ------------- */

trace_print(X) :-
	trace_printing(on),!,
	print(X).
trace_print(_X).

/* --------- */
/* trace_nl/ */
/* --------- */

trace_nl :-
	trace_printing(on),!,
	nl.
trace_nl.

trace_check_point(X) :-
        trace_printing(on),!,
	print(X),nl,
	print('### => Tracing Check Point:  => '),
       read(_Answer).
trace_check_point(_X).

/* ----------------------------------------------------- */


/* VERBOSE PRINTING */
/* by default on for interactive mode; off for CLI mode */


:- dynamic verbose_printing/1.

verbose_printing(on).


/* -------------------- */
/* set_verbose_printing/1 */
/* -------------------- */

set_verbose_printing(_NewVal) :-
	retract(verbose_printing(_Cur)),
	fail.
set_verbose_printing(NewVal) :-
	asserta(verbose_printing(NewVal)).




/* ------------- */
/* trace_print/1 */
/* ------------- */

verbose_print(X) :-
	verbose_printing(on),!,
	print(X).
verbose_print(_X).


verbose_println(X) :-
	verbose_printing(on),!,
	print(X), nl.
verbose_println(_X).

/* --------- */
/* trace_nl/ */
/* --------- */

verbose_nl :-
	verbose_printing(on),!,
	nl.
verbose_nl.


