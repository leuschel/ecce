%==============================================================
%#################### MODULE: PROG-SET-OP #####################
%==============================================================
% Provides a difference operation. Supports the shorten module.
% procP and defP correspond to the Definitions in John Gallagh-
% er's LOPSTR'97 paper (LNCS 1463, pp.282-299, Springer 1998).
%==============================================================
%---(C)-Stefan-Gruner-University-of-Southampton-England-2002---
%==============================================================

:- module(progSetOp, [defP/3, progDifference/3]).

:- use_module(library(lists)).
:- use_module(library(terms)).
:- use_module(auxil,[pruneRCD/2]).

%==============================================================
%##############################################################
%==============================================================

defP(TypeP, Prog, DefP) :-

	PredP =.. [TypeP,_],
	!,
	pruneRCD(rul__constraint__declaration([PredP],Prog),
		 rul__constraint__declaration([PredP],DefP)).

%--------------------------------------------------------------
%%%%%%% progDifference(A,B,D) : D=A\B. (A minus B is D) %%%%%%% 
%--------------------------------------------------------------

progDifference(Prog,[],Prog) :- !.

progDifference([],_,[]) :- !.

progDifference([Proc|ProgA], ProgB, ProgD) :-

	identifyProc(Proc, ProgB),
	!,
	progDifference(ProgA, ProgB, ProgD),
	!.

progDifference([Proc|ProgA], ProgB, [Proc|ProgD]) :-

	progDifference(ProgA, ProgB, ProgD).

%---------------------------------------------------------------

identifyProc(_,[]) :- !, fail.

identifyProc(DefA, [DefB|_]) :-

	copy_term(DefA, DefX),
	copy_term(DefB, DefX),
	!.

identifyProc(DefA, [_|ProgB]) :-

	identifyProc(DefA,
		     ProgB).

%---------------------------------------------------------------
%============================= END =============================
%---------------------------------------------------------------
