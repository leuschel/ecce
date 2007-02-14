%##############################################################
%################### MODULE: (NEW) SHORTENING #################
%##############################################################
% --(C)-Stefan-Gruner-University-of-Southampton-England-2002--

% ==========================================================
% John Gallaghers 1994 version of shortening is now replaced
% by his improved 1997 version of shortening,see Proc of the
% LOPSTR'97 pp.282-299 LNCS 1463 Springer-Verlag Berlin 1998.
% ==========================================================
% Already John's 1994 definition was a little bit sloppy and
% so is his 1997 definition of shortening as well. Implemen-
% ting his 1997 definition, I have assumed the following in-
% terpretation: "Let a predicate t depend on s, and let t, s
% have the same set of functionsymbols in there clause heads
% and let s be a subtype of t. Then those subgoals of type s
% shall be replaced by subgoals of type t which occur inside
% the full definition of t." I do hope that this is a proper
% interpretation of Johns sloppy definition! (In my opinion,
% any other interpretation wouldn't make much sense indeed.)
% ==========================================================

:- module(shorten, [shorten_LOPSTR_1997/4]).

:- use_module(self_check_rul).

:- use_module(library(lists)).
:- use_module(library(terms)).

:- use_module(compressor,
	      'compressor.pl',
	      [compress/2]).

:- use_module(prePostCon,
	      'prePostCon.pl',
	      [isRCD/1]).

:- use_module(domCallDepend,
	      'domCallDepend.pl',
	      [allCandidates/2]).

:- use_module(progSetOp,
	      'progSetOp',
	      [defP/3, progDifference/3]).

:- use_module(auxil,
	      'auxil.pl',
	      [pruneRCD/2, replaceAllOccurrences/5]).

%############################################################

:- initialization(assert_pre(shorten:shorten_LOPSTR_1997(InC,_,InP,_),
   prePostCon:isRCD(rul__constraint__declaration(InC,InP)))).

:- initialization(assert_post(shorten:shorten_LOPSTR_1997(_,OutC,_,OutP),
   prePostCon:isRCD(rul__constraint__declaration(OutC,OutP)))).

%===========================================================
% compress is called to eliminate ambiguous situations where-
% in two types s,s' are equivalent (subtypes of each other).
% ---------------------------------------------------------

shorten_LOPSTR_1997(ConstrIn, ConstrOut, ProgIn, ProgOut) :-

	print('START SHORTENING: PLEASE WAIT...'),
	!,
	compress(rul__constraint__declaration(ConstrIn,
					      ProgIn),
		 rul__constraint__declaration(ConstrOut,
					      WorkProg)),
	!,
	/* print('>>> Computing Shortening Candidates...'),
	nl,*/
	!,
	allCandidates(WorkProg, TypePairs),
	!,
	/* print('>>> Re-Directing Call-Chains...'),
	nl,*/
	!,
	cutCallChains(WorkProg, ConstrOut,
		      TypePairs, ProgOut),
	!,
	print('SHORTENING FINISHED'),
	nl.

%===========================================================
% pruneRCD is called to avoid broken links from being reest-
% ablished by later call-redirection steps. pruning is omit-
% ted in case that no call-redirection was previously done,
% which could be a consequence of previous pruning actions.
% .........................................................

cutCallChains(Prog, _,[], Prog) :- !.

cutCallChains(ProgIn, RefConstr,
	      [(Super,Sub)|Candidates], ProgOut) :-

	defP(Super, ProgIn, DefSuper),
	!,
	progDifference(ProgIn, DefSuper,
		       DontTouchTheRest),
	!,
	redirectCalls(Super,Sub, DefSuper,
		      NewDef, ACTION_FLAG),
	!,
	append(DontTouchTheRest,
	       NewDef, NewProg),
	!,
	pruning(ACTION_FLAG, RefConstr,
		NewProg, NextProg),
	!,
	cutCallChains(NextProg, RefConstr,
		      Candidates, ProgOut).
	
%===========================================================

redirectCalls(_,_, [],[], noReplacement) :- !.

redirectCalls(Super, Sub,
	      [proc(N/1, InputProcDef)|InputProgram],
	      [proc(N/1,OutputProcDef)|OutputProgram],
	      ACTION_FLAG) :-

	replaceAllOccurrences(Super, Sub,
			      ResultFlag,                
			      InputProcDef,            
			      OutputProcDef),
	!,
	redirectCalls(Super, Sub,
		      InputProgram,
		      OutputProgram,
		      HistoryFlag),
	!,
	showFlag(ResultFlag, HistoryFlag, ACTION_FLAG).

%-----------------------------------------------------------

showFlag(replacement,_,replacement) :- !.

showFlag(noReplacement, Flag, Flag) :- !.

%-----------------------------------------------------------

pruning(noReplacement, _, SameProg, SameProg) :- !.

pruning(replacement, Constr, ProgIn, ProgOut) :-

	pruneRCD(rul__constraint__declaration(Constr,
					      ProgIn),
		 rul__constraint__declaration(Constr,
					      ProgOut)).

%==============================================================
%##############################################################
%==============================================================

/*
test([a(X)],
     [proc(a/1,[(a(x):-true),(a(f(X)):-c(X)),(a(g(X)):-b(X))]),
      proc(b/1,[(b(x):-true),(b(f(X)):-c(X)),(b(g(X)):-a(X))]),
      proc(c/1,[(c(x):-true),(c(f(X)):-c(X)),(c(g(X)):-d(X))]),
      proc(d/1,[(d(f(X)):-c(X))]), %%]). %,
      proc(e/1,[(e(h(X)):-d(X))])]).*/

