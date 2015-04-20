% ----------------------------------------------------------------------
%
% Distributed CIAO, for SICStus 2.1.clp
% (C) UPM-CLIP 1995
%
% ----------------------------------------------------------------------

% ----------------------------------------------------------------------
% Builtin translation 
% ----------------------------------------------------------------------

ciao_expand_goal(Call,NewCall) :-
        ciao_global_command(Call), !,
	NewCall = execute_global(Call).
ciao_expand_goal(Call,Call).

ciao_global_command(abolish(_)).
ciao_global_command(abolish(_,_)).
ciao_global_command(abort).
ciao_global_command(assert(_)).
ciao_global_command(assert(_,_)).
ciao_global_command(asserta(_)).
ciao_global_command(asserta(_,_)).
ciao_global_command(assertz(_)).
ciao_global_command(assertz(_,_)).
ciao_global_command(close(_)).
ciao_global_command(compile(_)).
ciao_global_command(consult(_)).
ciao_global_command([_|_]).
ciao_global_command(ensure_loaded(_)).
ciao_global_command(fileerrors).
ciao_global_command(garbage_collect).
ciao_global_command(gc).
ciao_global_command(leash(_)).
ciao_global_command(load(_)).
ciao_global_command(load_foreign_files(_,_)).
ciao_global_command(maxdepth(_)).
ciao_global_command(module(_)).
ciao_global_command(nodebug).
ciao_global_command(nofileerrors).
ciao_global_command(nogc).
ciao_global_command(nospy _).
ciao_global_command(nospyall).
ciao_global_command(notrace).
ciao_global_command(op(_,_,_)).
ciao_global_command(open(_,_,_)).
ciao_global_command(open_null_stream(_)).
ciao_global_command(plsys(cd(_))).
ciao_global_command(plsys(cd)).
ciao_global_command(prolog_flag(_,_,_)).
ciao_global_command(prompt(_,_)).
ciao_global_command(reconsult(_)).
ciao_global_command(recorda(_,_,_)).
ciao_global_command(recorded(_,_,_)).
ciao_global_command(recordz(_,_,_)).
ciao_global_command(reinitialise).
ciao_global_command(restore(_)).
ciao_global_command(retract(_)).
ciao_global_command(retractall(_)).
ciao_global_command(spy _).
ciao_global_command(time_out(_,_,_)).
ciao_global_command(trace).
ciao_global_command(unix(cd(_))).
ciao_global_command(unix(cd)).
ciao_global_command(unknown(_,_)).
ciao_global_command(use_module(_)).
ciao_global_command(use_module(_,_)).
ciao_global_command(use_module(_,_,_)).

% ----------------------------------------------------------------------
% Expansion code
% ----------------------------------------------------------------------

% CIAO expansion of conjunctions and disjunctions:

% Conjunction
ciao_expand_body(','(G1,G2),','(NG1,NG2)) :-
	!,
	ciao_expand_body(G1,NG1),
	ciao_expand_body(G2,NG2).

% Disjunction
ciao_expand_body(';'(G1,G2),';'(NG1,NG2)) :-
	!,
	ciao_expand_body(G1,NG1),
	ciao_expand_body(G2,NG2).

% Single goal
ciao_expand_body(G,NG) :-
	!,
	ciao_expand_goal(G,NG).

%% Has to go afterwards or this file cannot be compiled!

% Calling expansion for

% Query
ciao_term_expansion((?- B),(?- NB)) :- 
	!,
	ciao_query_expansion(B,NB).

% Directive
ciao_term_expansion((:- B),(:- NB)) :- 
	!,
	ciao_expand_body(B,NB).

% Clause
ciao_term_expansion((H :- B),(H :- NB)) :- 
	!,
	ciao_expand_body(B,NB).

% Grammar rule (lets standard expansion work (but will not ciao_expand 
% bodies of grammar rules!)
ciao_term_expansion((_ --> _),_) :- 
	!,
	fail.

% Fact
ciao_term_expansion(F,F).

% Include also queries (not necessary on newer versions of sicstus, 
% but doesn't hurt and helps with ciao / sicstusclp 
% (otherwise query is not expanded)
ciao_query_expansion(X,Y) :- 
	ciao_expand_body(X,Y).

% Connection to global expansion
term_expansion(X,Y) :- ciao_term_expansion(X,Y).
query_expansion(X,Y) :- ciao_query_expansion(X,Y).

% Rules for clp(X) expansion should go here!
