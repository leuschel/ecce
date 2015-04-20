% ######################################################################
% Rough Regular Type Approximation Bottom Up Specialisation for one Atom
% ######################################################################

:- module(bottomUp, [get_bup_rulconstraints/2, get_bup_rulconstraints/3,
 default_input_file/1, default_temp_file/1,
  bottomUp/1, bottomUp/2, bottomUp/3]).
:- use_module(johnsTool, 'johnsTool.pl', [td/3]).
:- use_module(scanBUfile).


/* Main entry points for the bottom up package: */

get_bup_rulconstraints(QueryPredAtom,RULconstraint) :-
   copy_term(QueryPredAtom,QueryPredAtomCopy),
   bottomUp(QueryPredAtomCopy),
   scanBUprog(QueryPredAtom,
	   RULconstraint).
	   
get_bup_rulconstraints(QueryPredAtom,InputPath,RULconstraint) :-
   copy_term(QueryPredAtom,QueryPredAtomCopy),
   bottomUp(QueryPredAtomCopy,InputPath),
   scanBUprog(QueryPredAtom,
	   RULconstraint).


% to test: ?- get_bup_rulconstraints(test(X,Y),'test.pl',RUL).

% ######################################################################
% Writes the analysis result out into a per-default or user-defined file 
% ######################################################################

default_input_file('~/CVS/ecce/ecce_source/bottomUp/inp.pl').
default_temp_file('~/CVS/ecce/ecce_source/bottomUp/tmp.pl').

bottomUp(Atom) :- /* Default I/O-Paths */
	!,
	default_input_file(InFile),
	default_temp_file(TempFile),
	bottomUp(Atom,
		 InFile,
		 TempFile).

bottomUp(Atom, InputPath) :- /* Default O-Path */
	!,
	default_temp_file(TempFile),
	bottomUp(Atom,
		 InputPath,
		 TempFile).

bottomUp(Atom, InputPath, OutputPath) :-
	!,
	check(Atom),
	
	print('calling Johns tool < '),
	print(td(Atom, InputPath, OutputPath)),
	td(Atom, InputPath, OutputPath),
	print(' > done'),nl.

check(Atom) :-

	Atom =.. [P|A],
	functor(P,P,0),
	P \== ',',
	!.

check(Error) :-

	nl,
	print('# BottomUp INPUT ERROR:'),
	nl,
	print('# '),
	print(Error),
	print(' is no proper atom!'),
	nl,
	!,
	fail.

% ################################# END ################################
