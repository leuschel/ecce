/*             Copyright (C)1990-2002 UPM-CLIP				*/

/* ------------------------------------------------------

 HOW TO ADD A NEW KERNEL PREDICATE AND ITS INSTRUCTION:

    1.	Add a unit clause to inline_codable/1
	and open_code/5, saying how to compile
        a goal.

    2.	Add a clause to x_def_use/2
        to tell which temporaries the instruction
        defines and uses.

  ---------------------------------------------------- */

/* Changed all grammars in this file to plain prolog - PBC */

/* should be made into an argument that is passed around */
:- dynamic numberof_parcall_slots/1.

/* Size in number of cells of fixed part of environment devoted to 
   parcall frame.
   Total is this plus the space taken by the slots. See defs in wam.h */
empty_parcall_size(6).

%------------------------------------------------------------------------
% Output indexing code: this is only used as an indication in pwam output
% actual indexing is done in the loader
%------------------------------------------------------------------------

emit_all_predicates :-
	current_key_internal(_,K),		%% For each predicate...
	 recorded_internal(K,'$predicate'(Pred),Ref), 
	 erase(Ref),				%%  get it's definition and 
	 pl_pwam_in_progress,			%%  produce the appropriate
	 link_unit(Pred),			%%  output
	fail ;					%% Get the next predicate
	true.					%%  until there aren't any
                                                %% (succeeds if no pwam option)
/*===================================================
  Clause compiler

  Process next clause or directive from the input file.
  (Always FAIL, toploop will backtrack and read next term.)
  ===================================================*/

compile_all([X|Y]) :-
	compile_all(X),!,
	compile_all(Y).
compile_all([]).

% do nothing with the following:
compile_all(directive(succmode(_,_,_,_,_,_))). 
compile_all(directive(module(_,_))).
compile_all(directive(entry(_,_,_))).
compile_all(directive(entry(_,_))).
compile_all(directive(entry(_))).
compile_all(directive(trust(_,_,_))).
compile_all(directive(trust(_,_))).
compile_all(directive(exit(_,_))).
compile_all(directive(exit(_,_,_))).
compile_all(directive(parallelize)).
compile_all(directive(noparallelize)).

compile_all(directive(D)) :-		%% directives
	do_at_compile_time(D),
	do_at_eval_load_time(D).

compile_all(clause(H,B)) :-		%% dynamic predicates
	functor(H,F,A),
	dynamic_declaration(H,F/A),
        !,
	compile_file_emit(		%% interpreted (for now)
	call(assertz((H :- B)))).	%% ?? what about WAM output ?? R.W.

compile_all(clause(H,B)) :-		%% static predicates
	retractall(numberof_parcall_slots(_)),
	assert(numberof_parcall_slots(0)),
	parse_clause(H,B,Parsed),
	compile_internals(Parsed),
	retract(numberof_parcall_slots(_)).

% Object file printer. Produces different output depending on output option

compile_file_emit(Item) :-
	recorded_internal(compile_file_emit,'$emit'(Goal),_),
	arg(1,Goal,Item), !, 
	'QLCALL'(Goal).

/*===================================================
  Parsing

    Make second-level structures,
    open up disjunctions,
    take care of cut.
 ===================================================*/

parse_clause(H,B,Parsed) :-
	partition_clause(B,H,Parsed,[]),
	!.

partition_clause((P;Q), H, A, B) :-   	%% Toplevel disjunctions are recognized
        \+P=(_->_),                  	%%  as shorthand for several clauses.
        partition_clause(P, H, A, C),	%% Each parsed clause has its 
        partition_clause(Q, H, C, B).	%%  individual variables.
partition_clause(Body, H, A, B) :-
        functor(H, Fu, _),
        trans_args(1, H, Args, Dic),
        trans_term(Ch, Ch1, Dic),
        trans_goal(Body, PBody, '$cut'(Ch), Dic),
        'C'(A, parsed_clause(structure(Fu,Args),PBody,Ch1), B).

%---------------------
%  Translate goals
%---------------------

trans_goal(        V,S,Cut,Dic) :-		%% call(X)
	var(V),!,
	trans_goal(call(V),S,Cut,Dic).

trans_goal(   _^Goal,S,Cut,Dic) :-		%% Goal with existential vars
	trans_goal(Goal,S,Cut,Dic).

trans_goal(    false,S,Cut,Dic) :-		%% false -> fail
	trans_goal(fail,S,Cut,Dic).

trans_goal(otherwise,S,Cut,Dic) :-		%% otherwise -> true
	trans_goal(true,S,Cut,Dic).

trans_goal(     !,Tran,Cut,Dic) :-		%% cut
	trans_term(Cut,Tran,Dic).

trans_goal(   (X&Y),S,Cut,Dic) :-		%% AND PARALLEL
	structure(S,(X1&Y1)),
	inner_cut(Cut,Cut1),
	trans_goal(X,X1,Cut1,Dic),
	trans_goal(Y,Y1,Cut1,Dic).
/*
 *trans_goal(   (X//Y),S,Cut,Dic) :-			%% AND PARALLEL
 *	structure(S,(Fake,CGE)),
 *	inner_cut(Cut,Cut1),
 *	trans_goal('$CGE'(X,Y),Fake,Cut1,Dic),
 *	trans_AND_goal( (X//Y),CGE,Cut1,Dic).
 */

trans_goal(   (X\\Y),S,Cut,Dic) :-			%% OR PARALLEL
	structure(S,(X1\\Y1)),
	inner_cut(Cut,Cut1),
	trans_goal(X,X1,Cut1,Dic),
	trans_goal(Y,Y1,Cut1,Dic).

trans_goal(    (X;Y),S,Cut,Dic) :-			%% disjunction
	structure(S,(X1;Y1)),
	inner_cut(Cut,Cut1),
	trans_goal(X,X1,Cut1,Dic),
	trans_goal(Y,Y1,Cut1,Dic).

trans_goal(   (X->Y),S,Cut,Dic) :-			%% if-then-else
	structure(S,(X1->Y1)),
	inner_cut(Cut,Cut1),
	trans_goal(X,X1,Cut1,Dic),
	trans_goal(Y,Y1,Cut1,Dic).

trans_goal(      \+X,S,Cut,Dic) :-			%% not
	structure(S,\+X1),
	inner_cut(Cut,Cut1),
	trans_goal(X,X1,Cut1,Dic).

trans_goal(    (X,Y),S,Cut,Dic) :-			%% conjunction
	structure(S,(X1,Y1)),
	trans_goal(X,X1,Cut,Dic),
	trans_goal(Y,Y1,Cut,Dic).

%% Instructions not implemented yet... using Prolog versions 
%% in intrinsics3.pl (MH), to change, uncomment also def. in
%% "open_code", and in "inline_codable"
%%
%% trans_goal( ground([T]),structure(ground,Args),_,Dic) :-
%% 	!,trans_args(1,ground(T),Args,Dic).
%% 
%% trans_goal( ground([T1,T2]),structure(ground,Args),_,Dic) :-
%% 	!,trans_args(1,ground(T1,T2),Args,Dic).
%% 
%% trans_goal( ground([T1,T2,T3]),structure(ground,Args),_,Dic) :-
%% 	!,trans_args(1,ground(T1,T2,T3),Args,Dic).
%% 
%% trans_goal( ground([T1,T2,T3|Ts]),S,Cut,Dic) :-
%% 	!,structure(S,(Ta,Tb)),
%% 	Ta = structure(ground,Args),
%% 	trans_args(1,ground(T1,T2,T3),Args,Dic),
%% 	trans_goal( ground(Ts),Tb,Cut,Dic).
%% 
%% 
%% trans_goal( indep([[Ta,Tb]]),structure(indep,Args),_,Dic) :-
%% 	!,trans_args(1,indep(Ta,Tb),Args,Dic).
%% 
%% trans_goal( indep([[Ta,Tb]|Ts]),S,Cut,Dic) :-
%% 	!,structure(S,(T1,T2)),
%% 	T1 = structure(indep,Args),
%% 	trans_args(1,indep(Ta,Tb),Args,Dic),
%% 	trans_goal( indep(Ts),T2,Cut,Dic).

trans_goal(X, structure(Fu, Args), _, Dic):-
	recorded_internal('$checks_option',Flag,_),     %% not very efficient!
	check_native(Flag, X, Check),
	functor(Check, Fu, _),
	trans_args(1, X, Args, Dic).

trans_goal( X,structure(Fu,Args),_,Dic) :-		%% single goal
	functor( X,Fu,_),
	trans_args(1,X,Args,Dic).

%-------------------------
% Checks translated to ...
%-------------------------

check_native(interpreted,X,X).
check_native(native,X,Y):- native_check(X,Y).

native_check(ground(X),nground(X)).
native_check(indep(X),nindep(X)).
native_check(indep(X,Y),nindep(X,Y)).

%---------------------
% Translate arguments
%---------------------

trans_args(I,Term,[X1|Args],Dic) :-		%% Get the Ith argument
	arg(I,Term,X),				%%  of this term and look
	trans_term(X,X1,Dic),			%%  at it as a term...
	!,					%%
	I1 is I+1,				%% Get the next arg
	trans_args(I1,Term,Args,Dic).		%%  and recurse until
trans_args(_,_,[],_).				%% done!


%---------------------
% Translate terms
%---------------------

trans_term(V,Tran,Dic) :-			%% Is the term a
	var(V),					%%  variable...
	!,
	dic_lookup(Dic,V,Tran),
	Tran=var(_,_).

trans_term([X|Y],list(X1,Y1),Dic) :-		%% Is the term a
	!,					%%  List...
	trans_term(Y,Y1,Dic),
	trans_term(X,X1,Dic).

trans_term([],nil,_) :- !.			%% Is it the empty list...
trans_term(C,Tran,_) :- 			%% Is it a constant...
	atomic(C),
	!,
	Tran=constant(C).

trans_term(X,structure(Fu,Args),Dic) :- 	%% or a structure...
	functor(X,Fu,_),
	trans_args(1,X,Args,Dic).


/*===================================================
  Given a set of tokens (from the parser), transform
   them into actual compiled codes
 ===================================================*/

compile_internals([]).
compile_internals([parsed_clause(H,B,Choice)|Cs]) :-
	get_clause_id(H,ClauseName,F),
	record_internal(ClauseName,F),
	compile_one_proc(H,B,Choice,ClauseName,F),
	!,
	compile_internals(Cs).

compile_one_proc(Head,Body,Choice,Pred/No,F) :-
   transform_clause(Head,Body,Choice,TypeKey0,Body1,Pred/No,Internals,[]),
   compile_internals(Internals),
   trans_clause(Body1,FinalCode,TypeKey0,TypeKey1,EffAr),
   recorda_internal(F,'$compiled'(Pred/No,TypeKey1,EffAr),_),
   TypeKey1=type_key(T0,K0,C0),
   mode_declaration(F,Pred,_,M),
   mode_filter(M,T1),
   pred_filter(F,Pred,T2),
   set_pred_filter(F,Pred,T0,K0,C0,T2),
   T is T0/\T1/\T2,
   !, compile_file_emit(clause(Pred/No,FinalCode,type_key(T,K0,C0),EffAr)).


/*===================================================
  Given a clause, break off separate predicates for 
  special forms:
       disjunctions,  negations, implications.
 ===================================================*/

transform_clause(Hd,Body0,Choice,TypeKey,Body,ClName,A,B):-
	straight_goal(Body0,1,_,Hd,
			type_key(2'11111,nohash,nocut),
			TypeKey,Body1,[]),
	transform_clause_1(Hd,Body1,Choice,Body,ClName,A,B).

%---------------------
%  1st pass
%---------------------

straight_goal(S,Gn,Gn,_,TypeKey,TypeKey,A,B) :-
	structure(S,true),
	B=A.
straight_goal(S,Gn0,Gn,Head,TypeKey0,TypeKey,A,B) :-
	structure(S,(S0,S1)),
	straight_goal(S0,Gn0,Gn1,Head,TypeKey0,TypeKey1,A,C),
	straight_goal(S1,Gn1,Gn,Head,TypeKey1,TypeKey,C,B).
straight_goal(S,1,Gn,structure(_,Args),TypeKey0,TypeKey,A,B) :-
	trivial_head(Args,[]),
	( structure(S, '$cut'(_)), !,
	  Gn=1,
	  B=A
	; structure(S,'$dcut'(_)), !,
	  Gn=2,
	  'C'(A, goal(1,_,S), B)
        ),
   	TypeKey0=type_key(Type,Key,nocut),
	TypeKey=type_key(Type,Key,cut).
straight_goal(S,1,Gn,structure(_,Args),TypeKey0,TypeKey,A,B) :-
	structure(S,F),
	type_ck(F,Args,Type1),
	TypeKey0=type_key(Type0,Key,nocut),
	TypeKey=type_key(Type,Key,nocut),
	( trivial_head(Args,[]), !,
	  Gn=1,
	  Type is Type0/\Type1,
	  B=A
	; Gn=2,
	  Type is Type0/\ (Type1\/1),
	  'C'(A, goal(1,_,S), B)
	).
straight_goal(S,Gn,Gn1,_,TypeKey,TypeKey,A,B) :-
        'C'(A, goal(Gn,_,S), B),
   	Gn1 is Gn+1.


%---------------------
%  2nd pass
%---------------------

transform_clause_1(Hd,[],_,[goal(0,_,Hd)],_,A,B) :- !,
	allocate_temps(Hd),
	B=A.
transform_clause_1(Hd,Body1,Choice,Body,ClName,A,B) :-
	structure(S,'$choice'(Choice)),
	HeadGoal=goal(0,_,Hd),
	ChoiceGoal=goal(0,_,S),
	mk_occurrences_dic([HeadGoal,ChoiceGoal|Body1],Dic,0,Yn),
	straight_goal_1(Body1,Dic,Body2,ClName,A,B),
	Body=[HeadGoal,ChoiceGoal|Body2],
	allocate_vas(Dic,Body), 
	!,
	allocate_perms(Body,Yn,_).

straight_goal_1([], _, [], _, A, B) :-
        B=A.
straight_goal_1([goal(Gn,ESiz,Fu)|Gs],Dic,[goal(Gn,ESiz,Fu1)|Gs1],ClName,A,B):-
	straight_goal_2(Fu,Dic,Gn,Fu1,ClName,A,C), !,
	straight_goal_1(Gs,Dic,Gs1,ClName,C,B).

straight_goal_2(X,Dic,Gn,Y,ClName,A,B) :-
	structure(X,(_\\_)),
	structure(Y,or_par(S)),
	internal_predicate(Dic,Gn,S,ClName),
	!,
	straight_par_disj(X,S,A,B).
straight_goal_2(X,Dic,Gn,S,ClName,A,B) :-
	( structure(X,(_;_)); structure(X,(_->_))),
	internal_predicate(Dic,Gn,S,ClName),
	!,
	straight_disj(X,S,A,B).
straight_goal_2(X,Dic,Gn,S,ClName,A,B) :-
	structure(X,\+X1),
	internal_predicate(Dic,Gn,S,ClName),
	!,
	straight_neg(X1,S,A,B).
straight_goal_2(F,_,_,F,_,A,A).

straight_neg(G,Head,A,B) :-				%% Negation as failure
	structure_copy(G,G1,Dic),			%% not(X) :==
	structure_copy(Head,Head1,Dic),			%%  ( call(X),
	structure(B1,'$cut'(var(C1,C2))),		%%    !,
	structure(B2,fail),				%%    fail
	structure(B3,(B1,B2)),				%%  ;
	structure(S1,(G1,B3)),				%%    true
	structure(S2,true),				%%  )
	fake_head(Head1,Head2),
        'C'(A, parsed_clause(Head1,S1,var(C1,C2)), C),
        'C'(C, parsed_clause(Head2,S2,var(_,_)), B).

straight_disj(S,_,A,B) :-				%% Disjunctions
	structure(S,fail), 				%%
	B=A.             				%%
straight_disj(S,Head,A,B) :-				%%  break into separate
	structure(S,(S0;S1)),				%%  clauses...
	straight_disj(S0,Head,A,C),
	straight_disj(S1,Head,C,B).
straight_disj(S,Head,A,B) :-				%% If-then-else
	structure(S,(If->Then)),			%%  are rewriten as
	structure_copy(If,If1,Dic),			%%  disjunctions
	structure_copy(Then,Then1,Dic),			%%  with the IF
	structure_copy(Head,Head1,Dic),			%%  followed by a
	structure(B1,'$cut'(var(C1,C2))),		%%  CUT ("!")
	structure(B2,(B1,Then1)),
	structure(S1,(If1,B2)),
        'C'(A, parsed_clause(Head1,S1,var(C1,C2)), B).
straight_disj(S,Head,A,B) :-
	structure_copy(S,S1,Dic), 
	structure_copy(Head,Head1,Dic),
        'C'(A, parsed_clause(Head1,S1,var(_,_)), B).

straight_par_disj(S,_,A,B) :-				%% OR-parallel clauses
	structure(S,fail),				%% (handled as normal
	B=A.              				%%  disjunctions)
straight_par_disj(S,Head,A,B) :-
	structure(S,(S0\\S1)),
	straight_par_disj(S0,Head,A,C),
	straight_par_disj(S1,Head,C,B).
straight_par_disj(S,Head,A,B) :-
	structure_copy(S,S1,Dic), 
	structure_copy(Head,Head1,Dic),
        'C'(A, parsed_clause(Head1,S1,var(_,_)), B).


%---------------------
% Misc. support code
%---------------------

trivial_head([],_).
trivial_head([X|Xs],Seen) :-
	X=var(_,_),
	nocontainsx(Seen,X),
	trivial_head(Xs,[X|Seen]).

:- mode type_ck(+,?,?).

type_ck(var(X),   [Y|_],2'1)     :- X==Y.
type_ck(nonvar(X),[Y|_],2'11110) :- X==Y.
type_ck(atom(X),  [Y|_],2'100)   :- X==Y.
type_ck(atomic(X),[Y|_],2'110)   :- X==Y.
type_ck(number(X),[Y|_],2'10)    :- X==Y.
type_ck((\+ S0),Args,Code) :-
	structure(S0,F0),
	type_ck(F0,Args,Code0),
	Code is 2'11111-Code0.
type_ck((S0,S1),Args,Code) :-
	structure(S0,F0),
	structure(S1,F1),
	type_ck(F0,Args,Code0),
	type_ck(F1,Args,Code1),
	Code is Code0/\Code1.
type_ck((S0;S1),Args,Code) :-
	structure(S0,F0),
	structure(S1,F1),
	type_ck(F0,Args,Code0),
	type_ck(F1,Args,Code1),
	Code is Code0\/Code1.


internal_predicate(Dic,Gn,structure(Pred-_,Shared),Pred/_) :-
	linking_vars(Dic,Gn,Shared,[]).

fake_head(structure(F,H1),structure(F,H2)) :-
	fake_args(H1,H2).

fake_args([],[]).
fake_args([_|H1],[var(_,_)|H2]) :- fake_args(H1,H2).

linking_vars(D,_,A,B) :- var(D), !, B=A.
linking_vars(dic(V,Occs,L,R),Gn,A,B) :-
	linking_vars(L,Gn,A,C),
	( nonsingle(Occs),
	  set_in_ro(Gn,Occs), 
	  'C'(C, V, D)
	; D=C
	), !,
	linking_vars(R,Gn,D,B).

/*===================================================

  Compute environment sizes,
  allocate perm. variables.

  ===================================================*/

mk_occurrences_dic([],_,N,N).
mk_occurrences_dic([goal(Gn,_,G)|Gs],D,Yi,Yo) :-
	check_if_parcall(G,Yi,Yn),
	record_occurrences(G,Gn,D), 
	mk_occurrences_dic(Gs,D,Yn,Yo).

check_if_parcall(structure(&,Calls),Yi,Yo) :-
	empty_parcall_size(NumberofSlots),
	number_of(Calls,N),
	numberof_parcall_slots(M),
	M1 is M+NumberofSlots+N,
	retract(numberof_parcall_slots(M)),
	assert(numberof_parcall_slots(M1)),
	Yo is Yi+NumberofSlots+N.
check_if_parcall(_,N,N).

% was [A,_] ... MH & KG
number_of([_,A],N) :-  
	(
	    A = structure(&,Args),
	    number_of(Args,M),
	    N is M+1
	;   N = 2
        ).
number_of([_],1).


%---------------------
% Record occurrences 
%---------------------
:- mode record_occurrences(+,?,?).

record_occurrences(Var,Gn,D) :-
	Var=var(_,_),
	dic_lookup(D,Var,Occs),
	member(Gn,Occs).
record_occurrences(list(X,Y),Gn,D) :-
	record_occurrences(X,Gn,D),
	record_occurrences(Y,Gn,D).
record_occurrences(structure(_,Args),Gn,D) :-
	record_occurrences_args(Args,Gn,D).
record_occurrences(nil,_,_).
record_occurrences(constant(_),_,_).

%---------------------
% Record arguments
%---------------------
record_occurrences_args([],_,_).
record_occurrences_args([X|Xs],Gn,D) :-
	record_occurrences(X,Gn,D), 
	record_occurrences_args(Xs,Gn,D).

%---------------------
% Storage allocation
%---------------------
						%% CLASSIFY INTO CATEGORIES:

:- mode allocate_temps(+).			%% TEMPORARY storage
						%% *********
allocate_temps(nil).				%% nil
allocate_temps(constant(_)).			%% constants
allocate_temps(var(_,'x'(_))).			%% x_vars
allocate_temps(list(X,Y)) :-			%% lists
	allocate_temps(X),
	allocate_temps(Y).
allocate_temps(structure(_,Args)) :-		%% structures
	allocate_temps_args(Args).

allocate_temps_args([]).			%% recursively check
allocate_temps_args([X|Xs]) :-			%% the arguments of
	allocate_temps(X), 			%% structures...
	allocate_temps_args(Xs).

						%% and
allocate_perms([],Nv,Nv).			%% PERMANANT storage
allocate_perms([goal(_,Nv1,G0)|G1],Nv0,Nv) :-	%% *********
	allocate_perms(G1,Nv0,Nv1),!,
	allocate_perms_2(G0,Nv1,Nv).
 
:- mode allocate_perms_2(+,?,?).

allocate_perms_2(var(_,'y'(P)),Nv0,Nv) :-	%% y_vars
	var(P),	!,
	P=Nv0,
	Nv is Nv0+1.
allocate_perms_2(list(X,Y),Nv0,Nv) :-		%% lists
	!,
	allocate_perms_2(X,Nv0,Nv1),
	allocate_perms_2(Y,Nv1,Nv).
allocate_perms_2(structure(_,Args),Nv0,Nv) :-	%% structures
	!,
	allocate_perms_2_args(Args,Nv0,Nv).
allocate_perms_2(_,Nv,Nv).

allocate_perms_2_args([],Nv,Nv).		%% recursively check
allocate_perms_2_args([X|Xs],Nv0,Nv) :-		%% the arguments of
	allocate_perms_2(X,Nv0,Nv1),		%% structures...
	allocate_perms_2_args(Xs,Nv1,Nv).

%---------------------
% Do the allocation
%---------------------

allocate_vas(D,_) :- var(D), !.
allocate_vas(dic(var(_,Name),Occs,L,R),Body) :-
	allocate_vas(L,Body),
	allocate_vas_3(Name,Occs,Body),
	allocate_vas(R,Body).
 
allocate_vas_3('y'(_),Occs,[_|Goals]) :-
	spans_hard_call(Occs,Goals).
allocate_vas_3('x'(_),_,_).


spans_hard_call(Occs,[G|Gs]) :-
	(
	    inline_codable_goal(G,_)
	;   goalno_before_occs(G,Occs)
        ), !,
	spans_hard_call(Occs,Gs).

spans_hard_call(Occs,[goal(Hard,_,_)|_]) :-
	spans_hard_goal_1(Occs,Hard),!.
spans_hard_call(Occs,[goal(Hard,_,structure(&,_))|_]) :-
	spans_andpar_goal(Occs,Hard).


spans_andpar_goal([O|Os],Hard) :-
	O>=Hard,!;
	nonvar(Os),
	spans_andpar_goal(Os,Hard).

spans_hard_goal_1([O|Os],Hard) :-
	O>Hard, !;
	nonvar(Os),
	spans_hard_goal_1(Os,Hard).

goalno_before_occs(goal(Gn,_,_),[O|_]) :- Gn<O.


/*===================================================
   Compile an internal predicate: Compile all clauses
   and link them together.
  ===================================================*/

index_predicate(Pred,A,B) :-
	get_key(Pred,F),
	collect_clauses(F,Pred,0,EffAr,Clauses,[]),
	parallel_declaration(F,Pred,Flag),
	mode_declaration(F,Pred,Mode,M),
	parallelize(Flag,A,C),
	emit_wait(Mode,1,C,D),
	link_group(Clauses,EffAr,M,D,B).


emit_wait(Mode,I,A,B) :-			%% Wait instructions 
	I1 is I+1,				%%  are governed by 
	arg(I1,Mode,M),				%%  mode declaration.
	emit_wait_1(M,wait_x(I),A,C),
	!,
	emit_wait(Mode,I1,C,B).
emit_wait(_,_,A,A).


:- mode emit_wait_1(+,?,?,?).

emit_wait_1(?,_,A,A).			        %% Check the mode...
emit_wait_1(!,I,A,B) :-
        'C'(A, I, B).
emit_wait_1(-,_,A,A).
emit_wait_1(+,_,A,A).


collect_clauses(F,Pred,Max0,Max,A,B) :-
	recorded_internal(F,Instance,Ref), 
	Instance='$compiled'(Pred/_,_,Max1),
	erase(Ref),
	max_of([Max1],Max0,Max2),
	collect_clauses(F,Pred,Max2,Max,A,C),
	!,
	'C'(C,Instance,B).
collect_clauses(_,_,Max,Max,A,A).

max_of([],Y,Y).
max_of([X|Xs],Y0,Y) :-
	X@=<Y0,
	!,
	max_of(Xs,Y0,Y);
	max_of(Xs,X,Y).

%:- mode parallelize(+).
:- mode parallelize(+,?,?).

parallelize(0,A,A).
parallelize(1,A,B):-
        'C'(A, execute_or(label(L)), C),
        'C'(C, label(L), B).

/*===================================================
  Link i.e. index all the clauses.
  ===================================================*/

link_group(Cs,Arity,?,A,B) :-
	appropriate_for_any(Cs,Cs1),		%% a cut could 
	'C'(A,execute(L),C),			%% render some clauses
	link_noswitch(Cs1,L,Arity,_,C,B).	%% inaccessible.
link_group(Cs,Arity,Mode,A,B) :-
	'C'(A,switch_on_term(Lvar,Lnum,Latm,Llist,Lstruct),C),
	appropriate_1(2'1,Cs,CsvarD,Mode),
	appropriate(2'10,Cs,Csnum,CsnumD,Mode),
	appropriate(2'100,Cs,Csatm,CsatmD,Mode),
	appropriate_1(2'1000,Cs,CslistD,Mode),
	appropriate(2'10000,Cs,Csstruct,CsstructD,Mode),
	link_noswitch(CsvarD,Lvar,Arity,Dic,C,C1),
	link_noswitch(CslistD,Llist,Arity,Dic,C1,C2),
	link_noswitch(CsnumD,LnumD,Arity,Dic,C2,C3),
	link_noswitch(CsatmD,LatmD,Arity,Dic,C3,C4),
	link_noswitch(CsstructD,LstructD,Arity,Dic,C4,C5),
	link_switch(Csnum,Lnum,Arity,switch_on_constant(_,LnumD),Dic,C5,C6),
	link_switch(Csatm,Latm,Arity,switch_on_constant(_,LatmD),Dic,C6,C7),
	link_switch(Csstruct,Lstruct,Arity,switch_on_structure(_,LstructD),
	                                                         Dic,C7,B).

:- mode link_noswitch(+,?,?,?,?,?).

link_noswitch([],L,_,Dic,A,B) :-
	dic_lookup(Dic,[],L),
	( nonvar(L),
	  B=A
	; link_fail(L,A,B)
        ).
link_noswitch('$modex',L,_,Dic,A,B) :-
	dic_lookup(Dic,'$modex',L),
	( nonvar(L),
	  B=A
	; link_modex(L,A,B)
        ).
link_noswitch('$waitx',L,_,Dic,A,B) :-
	dic_lookup(Dic,'$waitx',L),
	( nonvar(L),
	  B=A
	; link_waitx(L,A,B)
        ).
link_noswitch(Cs,L,A,Dic,B0,B) :-
	dic_lookup(Dic,Cs,L),
	( nonvar(L),
	  B=B0
	; link_try_seq(Cs,L,A,B0,B)
        ).


link_fail(label(L),A,B) :-
	'C'(A, label(L), C),
        'C'(C, fail, B).

link_modex(label(L),A,B) :-
        'C'(A, label(L), C),
        'C'(C, mode_error_x0, B).

link_waitx(label(L),A,B) :-
        'C'(A, label(L), C),
        'C'(C, wait_x0, B).


link_switch([],Default,_,Insn,_,A,B) :-
	arg(2,Insn,Default),
	B=A.
link_switch(Alist,label(Label),Arity,Insn,Dic,A,B) :-
	'C'(A,label(Label),C),
	'C'(C,Insn,D),
	arg(1,Insn,Table),
	switch_table(Alist,Table,Arity,Dic,D,B).

switch_table([],[],_,_,A,A).
switch_table([[hash(K)|Clauses]|Alist],[[K,Label]|Table],Arity,Dic,A,B) :-
	link_noswitch(Clauses,Label,Arity,Dic,A,C),
	switch_table(Alist,Table,Arity,Dic,C,B).


link_try_seq([C],Label,_,A,B) :-
	clause_label(C,Label),
	B=A.
link_try_seq([C1|Cs1],label(Label),Arity,A,B) :-
	'C'(A,label(Label),C),
	'C'(C,try(OL,Arity),D),
	clause_label(C1,OL), 
	link_try_c(Cs1,Arity,D,B).


link_try_c([C],Arity,A,B) :-
	'C'(A,trust(OL,Arity),B),
	clause_label(C,OL).
link_try_c([C|Cs],Arity,A,B) :-
	'C'(A,retry(OL,Arity),B0),
	clause_label(C,OL),
	link_try_c(Cs,Arity,B0,B).

%-----------------------
% Whatever's appropriate
%-----------------------

appropriate_for_any([],[]).
appropriate_for_any([C|_],[C]) :-
	type_of_clause(C,2'11111),
	last_clause_for_key(C,2'1), !.
appropriate_for_any([C|Cs],[C|Out]) :-
	type_of_clause(C,2'11111),
	appropriate_for_any(Cs,Out).

:- mode app_mode(+,?,?).

app_mode(+,2'1,'$modex').
app_mode(!,2'1,'$waitx').
app_mode(-,T  ,'$modex') :- T>2'1.

appropriate(Type,Clauses,Alist,Default1,Mode) :-
	app_mode(Mode,Type,Default1), !,
	Alist=[];
	appropriate_clauses_dic(Type,Clauses,Dic),
	appropriate_clauses_dic_traverse(Dic,Alista,[],Default),
	merge_clause_lists([],Default,Default1,Type),
	merge_alist(Alista,Default,Alist,Type).

appropriate_1(Type,Csin,Csout,Mode) :-
	app_mode(Mode,Type,Csout), !;
	appropriate_2(Type,Csin,Csout).

appropriate_2(_,[],[]).
appropriate_2(Type,[C|Cs],[C|Out]) :-
	appropriate_clause_1(Type,_,C), !,
	appropriate_3(Type,Cs,Out,C).
appropriate_2(Type,[_|Cs],Out) :- appropriate_2(Type,Cs,Out).

appropriate_3(Type,_,[],C) :- last_clause_for_key(C,Type).
appropriate_3(Type,Cs,Out,_) :- appropriate_2(Type,Cs,Out).

appropriate_clause_1(Type,Key,'$compiled'(_,type_key(Type1,Key,_),_)) :-
	Type/\Type1>0.

appropriate_clauses_dic(_,[],_).
appropriate_clauses_dic(Type,[C|Cs],Dic) :-
	(
	    appropriate_clause_1(Type,Key,C),
	    dic_lookup(Dic,Key,KeyCs),!,
	    member(C,KeyCs)
	;   true
        ),
	appropriate_clauses_dic(Type,Cs,Dic).


appropriate_clauses_dic_traverse(Dic,AL0,AL,_) :-
	var(Dic),!,
	AL0=AL.
appropriate_clauses_dic_traverse(dic(nohash,Cs,L,R),AL0,AL,Cs) :- !,
	appropriate_clauses_dic_traverse(L,AL0,AL1,Cs),
	appropriate_clauses_dic_traverse(R,AL1,AL,Cs).
appropriate_clauses_dic_traverse(dic(Key,Cs,L,R),AL0,AL,Default) :-
	appropriate_clauses_dic_traverse(L,AL0,[[Key|Cs]|AL1],Default),
	appropriate_clauses_dic_traverse(R,AL1,AL,Default).

/*===================================================
   Must merge in variable clauses into each set of
   non-v clauses.  Take cuts into account too.
  ===================================================*/

merge_alist([],_,[],_).
merge_alist([[K|A]|As],D,[[K|B]|Bs],T) :-
	merge_clause_lists(A,D,B,T),!,
	merge_alist(As,D,Bs,T).

merge_clause_lists([],[],[],_).
merge_clause_lists([],[D1|Ds],[D1|Ct],Type) :-
	c_merge_clause_lists([],Ds,Ct,Type,D1).
merge_clause_lists([C1|Cs],[],[C1|Ct],Type) :-
	c_merge_clause_lists(Cs,[],Ct,Type,C1).
merge_clause_lists([C1|Cs],[D1|Ds],[C1|Ct],Type) :-
	C1@<D1,
	c_merge_clause_lists(Cs,[D1|Ds],Ct,Type,C1).
merge_clause_lists([C1|Cs],[D1|Ds],[D1|Ct],Type) :-
	C1@>D1,
	c_merge_clause_lists([C1|Cs],Ds,Ct,Type,D1).


c_merge_clause_lists(_,_,[],Type,C) :-
	last_clause_for_key(C,Type).
c_merge_clause_lists(L1,L2,L3,Type,_) :-
	merge_clause_lists(L1,L2,L3,Type).



type_of_clause(     '$compiled'(_,type_key(T,_,_),_),T).
last_clause_for_key('$compiled'(_,type_key(_,nohash,cut),_),_).
clause_label(       '$compiled'(Label,_,_),Label).

/*===================================================
  Translating a clause:

	Emit naive code,
	then allocate temps,
	then postpone 'get_variable's till 'neck',
	finally optimize first arg. matching.

  ===================================================*/

trans_clause(Body,Code,TypeKey0,TypeKey,EffAr) :-
	trans_clause_1(Body,Envp,Code0,[]),
	extract_index(Code0,Code1,Code2,Code1,TypeKey0,TypeKey,0,M,Ys,Ys),
	clause_lifetime(Code2,Live,Code3),
	max_of(Live,M,MaxLive),
	EffAr is MaxLive+1,
	!,
	peep_clause(Envp,Code3,Code,[]).

trans_clause_1([goal(0,_,Head)|Body],Envp,A,B) :-	%% Translate a clause: 
	hard_arglist(Body,Aft),			        %%   first head, 
	( Aft=[], Envp=no; Envp=yes),			%%   then body. 
	c_head_args(Head,Dic,Arity,A,C), !,		%% Note whether we 
	c_guards(Body,Arity,Dic,C,B).			%%   need an env.

hard_arglist([],[]).
hard_arglist([G|_],[G]) :- G = goal(_,_,structure(&,_)),!.
hard_arglist([G|Gs],Aft) :-
	inline_codable_goal(G,_),
	!,
	hard_arglist(Gs,Aft)
    ;   Aft=Gs.

inline_codable_goal(goal(_,_,G),Fu) :-
	safe_structure(G,Fu),
	inline_codable(Fu).

clause_lifetime([neck(N)|Code0],Live,[ifshallow,neck(N),else,endif|Code0]) :-
	guard_lifetime(Live,Code0,[]).
clause_lifetime(Code0,Live,Code) :-
	guard_and_body(Code0,G1,G2,B,_),
	guard_lifetime(Live,G1,[]),
	guard_lifetime(_,G2,[]),
	body_lifetime(_,B,[]),
	connect_guard_and_body(G1,G2,B,Code,[]).

guard_and_body([cut_x(A),neck(N)|Xs],[cut_x(A),neck(N)|Ys],Zs,Us,Dic) :-
	A== -1,	!,
	guard_and_body(Xs,Ys,Zs,Us,Dic).
guard_and_body([neck(N)|Xs],[neck(N)|Ys],Zs,Us,Dic) :-
	!,
	guard_and_body(Xs,Ys,Zs,Us,Dic).
guard_and_body([X|Xs],[X],[X],Xs,_) :-
	xfer_insn(X,_), !.
guard_and_body([cut_x(A)|Xs],Ys,Zs,Us,Dic) :-
	A== -1,	!,
	guard_and_body(Xs,Ys,Zs,Us,Dic).
guard_and_body([X|Xs],[X|Ys],[X1|Zs],Us,Dic) :-
	copy_insn(X,X1,Dic),
	!,
	guard_and_body(Xs,Ys,Zs,Us,Dic).

copy_insn(X,X1,Dic) :-
	functor(X,F,N),
	functor(X1,F,N),
	copy_insn_args(0,N,X,X1,Dic).

copy_insn_args(N,N,_,_,_) :- !.
copy_insn_args(N0,N,X,X1,Dic) :-
	N1 is N0+1,
	arg(N1,X,A),
	arg(N1,X1,A1),
	copy_var(A,A1,Dic),
	copy_insn_args(N1,N,X,X1,Dic).

copy_var(X,X1,Dic) :-
	var(X), dic_lookup(Dic,X,X1);
	nonvar(X), X=X1.

connect_guard_and_body(G1,G2,B,A0,A) :-
	'C'(A0,ifshallow,A1),
	dlist_guard(G1,G1a,A1,A2),
	length(G1a,L1a),
	length(G2,L2),
	some_insns(G1a,G1b,L1a,L1b,A2,A3),
	'C'(A3,else,A4),
	some_insns(G2,G1b,L2,L1b,A4,A5),
	!,
	'C'(A5,endif,A6),
	dlist(G1b,A6,A7),
	dlist(B,A7,A).

some_insns(Xs,Xs,L,L,A,A).
some_insns([X|Xs],Ys,L0,L,A,B) :-
	L0@>L,
	L1 is L0-1,
	'C'(A,X,C),
	some_insns(Xs,Ys,L1,L,C,B).

dlist_guard([X|Xs],Rest,A,B) :-
	'C'(A,X,C),
	dlist_guard(X,Xs,Rest,C,B).

dlist_guard(neck(_),Xs,Rest,A,B) :- dlist_guard_1(Xs,Rest,A,B).
dlist_guard(_,Xs,Rest,A,B) :- dlist_guard(Xs,Rest,A,B).

dlist_guard_1([X|Xs],Rest,A,B) :-
	'C'(A,X,C),
	dlist_guard_1(X,Xs,Rest,C,B), !.
dlist_guard_1(Rest,Rest,A,A).

dlist_guard_1(get_x_variable(_,_),Xs,Rest,A,B) :-
	dlist_guard_1(Xs,Rest,A,B).
dlist_guard_1(get_y_variable(_,_),Xs,Rest,A,B) :-
	dlist_guard_1(Xs,Rest,A,B).
dlist_guard_1(unify_x_variable(_),Xs,Rest,A,B) :-
	dlist_guard_1(Xs,Rest,A,B).
dlist_guard_1(unify_y_variable(_),Xs,Rest,A,B) :-
	dlist_guard_1(Xs,Rest,A,B).
dlist_guard_1(proceed,Xs,Rest,A,B) :-
	dlist_guard_1(Xs,Rest,A,B).
dlist_guard_1(execute(_),Xs,Rest,A,B) :-
	dlist_guard_1(Xs,Rest,A,B).


/*===================================================
   Extract index information from code i.e. from
   matching the first argument.

    extract_index( C0,		; Sequence of WAM insns,
		   C1,		; = C0, after removing 
				;   general indexable
				;   instruction, with all
				;   elements in Y0 moved
		   		;   to the end,
		   C2,		; Special indexable
		   		; instruction (if any) + C3,
		   C3,		;
		   TK0,		; 
		   TK,		; type and key info (TK0),
		   		;  updated by indexing insn,
		   X0,		; name of indexable argument,
		   M,		; Y0 = Y = all 'get_variable'
		   Y0,		; insns up to first call.
		   Y		;
	         ) :- ...

  ===================================================*/


extract_index(	Code0,Code,
		Final0,Final,
		TypeKey0,TypeKey,
		X0,M,Queue,Head) :-

	get_variable(_,_,Code0,Code1),
	first(Code0,Queue,Queue1),!,
        extract_index(Code1,Code,Final0,Final,TypeKey0,
	              TypeKey,X0,M,Queue1,Head).

extract_index(	Code0,Code,
		Final0,Final,
		TypeKey0,TypeKey,
		X0,M,Queue,Head) :-

	put_value(x(X),A,Code0,Code1),
	var(A),
	X=A,!,
	extract_index(Code1,Code,Final0,Final,TypeKey0,
                      TypeKey,X0,M,Queue,Head).

extract_index(	[I|Code0],Code,
		Final0,Final,
		TypeKey0,TypeKey,
		X0,0,Queue,Head) :-

	index_insn(I,I1,X0,Type1,Key1),
	Final0=[I1|Final],
	TypeKey0=type_key(Type0,nohash,nocut),
	TypeKey=type_key(Type,hash(Key1),nocut),
	Type is Type0/\Type1,!,
	extract_index_2(Code0,Code,Queue,Head).

extract_index(	Code0,Code,
		Final,Final,
		TypeKey,TypeKey,
		_,-1,Queue,Head) :-

	extract_index_2(Code0,Code,Queue,Head).


extract_index_2(Code0,Code,Queue,Head) :-
	get_variable(_,_,Code0,Code1),
	first(Code0,Queue,Queue1),!,
	extract_index_2(Code1,Code,Queue1,Head).

extract_index_2(Code0,Head1,Queue,Head) :-
	cut(_,Code0,Code1),
	first(Code0,Head1,[NECK|Head]),
	butneck(NECK,Queue,Code3,Code1,Code2),!,
	collapse_moves(Code2,Code3).

extract_index_2(Code0,[NECK|Head],Queue,Head) :-
	safe_insns(NECK,Queue,Code3,Code0,Code2),!,
	collapse_moves(Code2,Code3).

extract_index_2(In,Code,Queue,Head) :-
	put_value(x(X),A,In,In1),
	var(A),
	X=A,!,
	extract_index_2(In1,Code,Queue,Head).

extract_index_2([I|Code0],[I|Code],Queue,Head) :-
	extract_index_2(Code0,Code,Queue,Head).

:- mode index_insn(+,?,?,?,?).

index_insn(get_constant(K,X),get_constant_x0(K),X0,Type,Key) :- 
	X==X0, 
	type_of_constant(K,Type), 
	key_of_constant(K,Key).
index_insn(get_structure(S,X),get_structure_x0(S),X0,2'10001,S) :- X==X0.
index_insn(get_nil(X),get_nil_x0,X0,2'101,[]) :- X==X0.
index_insn(get_list(X),get_list_x0,X0,2'1001,'.'/2) :- X==X0.

butneck(neck(N),S,S,A,B) :-
	'C'(A,neck(N),B).
butneck(N,S1,S,A0,B) :-
	put_value(x(X),A,A0,A1),
	var(A), X=A, !,
	butneck(N,S1,S,A1,B).
butneck(N,[S0|S1],S,A,B) :-
	'C'(A,S0,C),
	butneck(N,S1,S,C,B).

safe_insns(neck(N),S,S,A,B) :-
	'C'(A,neck(N),B).
safe_insns(N,[S0|S1],S,A,B) :- 
	'C'(A,S0,C),
	safe_insn(S0),
	safe_insns(N,S1,S,C,B).

collapse_moves(In,Code) :-
	put_value(x(X),A,In,In1),
	var(A),
	X=A,!,
	collapse_moves(In1,Code).

collapse_moves([I|In],[I|Code]) :-
	xfer_insn(I,_), !,
	In=Code.
collapse_moves([I|In],[I|Code]) :-
	collapse_moves(In,Code).

%-----------------------
%  An incomplete list of
%  instructions that
%  can't fail:
%-----------------------
:- mode safe_insn(+).

safe_insn(get_x_variable(_,_)).
safe_insn(get_y_variable(_,_)).
safe_insn(put_x_variable(_,_)).
safe_insn(put_y_variable(_,_)).
safe_insn(unify_x_variable(_)).
safe_insn(unify_y_variable(_)).


:- mode type_of_constant(+,?).

type_of_constant(K,    2'00011) :- number(K), !.
type_of_constant(K,    2'00101) :- atom(K), !.
type_of_constant([_|_],2'01001) :- !.
type_of_constant(_,    2'10001).


:- mode key_of_constant(+,?).

key_of_constant(K,K)   :- atomic(K),!.
key_of_constant(K,F/L) :- functor(K,F,L).

%-----------------------
% Open coding of
% PROLOG expressions
%-----------------------
:- mode open_code(+,?,?,?,?).

open_code(fail,Dic,Dic,[fail|A],A).
open_code(available_siblings,Dic,Dic,[available_siblings|A],A).
% ---- Added for tracing... MH
% N1 is one less to encode 2 goals as opcode+1, 3 as opcode+2 etc. to 5
% See instrdefs.h
open_code('$trace_fork'(constant(N)),Dic,Dic,[trace_fork(N1)|A],A) :-
	N1 is N-1.
% N encodes end of goal 1 as opcode+1, 3 as opcode+2 etc. to 4
open_code('$trace_goal_success'(constant(N)),Dic,Dic,[trace_goal_success(N)|A],A).
open_code('$trace_join'(constant(N)),Dic,Dic,[trace_join(N1)|A],A) :-
	N1 is N-1.
%----------------------------------------------------------------

open_code('$label1',Dic,Dic,['$label1'|A],A).
open_code('$label1_no_label2',Dic,Dic,['$label1_no_label2'|A],A).
open_code('$label2',Dic,Dic,['$label2'|A],A).

%----------------------------------------------------------------

%% [MCL:BUILTIN_INC]

open_code('$inc'(X,Y),Dic0,Dic2) -->
	c_put_arg(X, Xreg, 1000, Dic0, Dic1),
	c_put_arg(Y, Yreg, 1000, Dic1, Dic2), !,
	[builtin_inc(Xreg, Yreg)].

%----------------------------------------------------------------


open_code('$choice'(var(g,Arg)),Dic,Dic,A,B) :-
	Arg='x'(-1), !,
	B=A.
open_code('$choice'(var(g,Arg)),Dic,Dic,A,B) :-
	cache_add(Dic,-1,var(g,Arg)),!,
	get_variable(Arg,-1,A,B).

open_code('$cut'(var(_,Arg)),Dic,Dic,A,B) :-
	cached_ref(Arg,Arg1,Dic),!,
	cut(Arg1,A,B).

open_code('$dcut'(var(_,Arg)),Dic,Dic,A,B) :-
	cached_ref(Arg,Arg1,Dic),!,
	dcut(Arg1,A,B).

open_code(X = Y,Dic0,Dic,A,B):- !,			%% "=" -> unify
	c_equal(X,Y,1000,Dic0,Dic,A,B).

open_code('C'(X,Y,Z),Dic0,Dic,A,B) :- !,
	c_equal(X,list(Y,Z),1000,Dic0,Dic,A,B).

open_code(Value is Expr,Dic0,Dic,A,B) :-		%% is/2
	c_expr(Expr,Dest,1000,Dic0,Dic1,A,C),!,
	c_equal(Dest,Value,1000,Dic1,Dic,C,B).


%% open_code(ground(X),Dic0,Dic)
%%    -->  c_put_arg(X,Xreg,1000,Dic0,Dic),!,
%% 	[ground(Xreg)].
%% open_code(ground(X,Y),Dic0,Dic)
%%    -->  c_put_arg(X,Xreg,1000,Dic0,Dic1),
%% 	c_put_arg(Y,Yreg,1000,Dic1,Dic),!,
%% 	[ground(Xreg,Yreg)].
%% open_code(ground(X,Y,Z),Dic0,Dic)
%%    -->  c_put_arg(X,Xreg,1000,Dic0,Dic1),
%% 	c_put_arg(Y,Yreg,1000,Dic1,Dic2),
%% 	c_put_arg(Z,Zreg,1000,Dic2,Dic),!,
%% 	[ground(Xreg,Yreg,Zreg)].
%% 
%% open_code(indep(X,Y),Dic0,Dic)
%%    -->  c_put_arg(X,Xreg,1000,Dic0,Dic1),
%% 	c_put_arg(Y,Yreg,1000,Dic1,Dic),!,
%% 	[indep(Xreg,Yreg)].
%% 
%% open_code(Builtin,Dic0,Dic)				%% pre_evaluated
%%    -->  {Builtin=..[Name,X,Y],				%% builtin/2
%% 	name_of_builtin(Name,2,_),
%% 	eval_builtin(Name)
%%         },
%% 	c_expr(Y,Y1,1000,Dic0,Dic1),
%% 	c_expr(X,X1,1000,Dic1,Dic2),
%% 	c_put_arg(X1,Xreg,1000,Dic2,Dic3),
%% 	c_put_arg(Y1,Yreg,1000,Dic3,Dic),!,
%% 	[builtin_2(Name,Xreg,Yreg)].

open_code(Builtin,Dic0,Dic,A,B)	:-			%% builtin/1
	Builtin=..[Name,X],
	name_of_builtin(Name,1,_),
	c_put_arg(X,Xreg,1000,Dic0,Dic,A,C),!,
	'C'(C,builtin_1(Name,Xreg),B).

open_code(Builtin,Dic0,Dic,A,B) :-
	Builtin=..[Name,X,Y],				%% builtin/2
	name_of_builtin(Name,2,_),
	c_put_arg(X,Xreg,1000,Dic0,Dic1,A,C),
	c_put_arg(Y,Yreg,1000,Dic1,Dic,C,D),!,
	'C'(D,builtin_2(Name,Xreg,Yreg),B).

open_code(Builtin,Dic0,Dic,A,B) :-			%% builtin/3
	Builtin=..[Name,X,Y,Z],
	name_of_builtin(Name,3,_),
	c_put_arg(X,Xreg,1000,Dic0,Dic1,A,A0),
	c_put_arg(Y,Yreg,1000,Dic1,Dic2,A0,A1),
	c_put_arg(Z,Zreg,1000,Dic2,Dic,A1,A2),!,
	'C'(A2,builtin_3(Name,Xreg,Yreg,Zreg),B).


c_expr(E,var(g,'x'(Vreg)),Size,Dic0,Dic,A,B) :-
	structure(E,E1),
	( E1=X-constant(1), N='SUB1'
	; E1=X+constant(1), N='ADD1'
	; E1=..[N,X], name_of_function(N,1,_)
	),
	c_expr(X,X1,Size,Dic0,Dic1,A,C),
	c_put_arg(X1,Xreg,Size,Dic1,Dic,C,D),!,
	'C'(D,function_1(N,Vreg,Xreg),B).

c_expr(E,var(g,'x'(Vreg)),Size,Dic0,Dic,A,B) :-
	structure(E,E1),
	E1=..[N,X,Y],
	name_of_function(N,2,_),
	c_expr(Y,Y1,Size,Dic0,Dic1,A,A0),
	c_expr(X,X1,Size,Dic1,Dic2,A0,A1),
	c_put_arg(X1,Xreg,Size,Dic2,Dic3,A1,A2),
	c_put_arg(Y1,Yreg,Size,Dic3,Dic,A2,A3),!,
	'C'(A3,function_2(N,Vreg,Xreg,Yreg),B).

c_expr(E,E,_,Dic,Dic,A,A).


cached_ref(Ref,Ref1,C) :-
	cache_search_var(C,_,Ref1,Ref)
    ;	Ref=Ref1.


c_equal(U,U1,_,Dic,Dic,A,B) :-
	U==U1,!,
	B=A.
c_equal(var(Du,U),var(Dv,V),_,Dic,Dic,A,B) :- !,
	c_eq_vars(U,V,Du,Dv,Dic,A,B).
c_equal(U,var(Dv,V),Size,Dic0,Dic,A,B) :-
	var(Dv),
	Dv=g,!,
	top_put(V,U,Size,Dic0,Dic,A,B).
c_equal(var(Du,U),V,Size,Dic0,Dic,A,B) :-
	var(Du),
	Du=g,!,
	top_put(U,V,Size,Dic0,Dic,A,B).
c_equal(U,var(_,N),_,Dic,Dic,A,B) :- !,
	cached_ref(N,N1,Dic),
	top_get(N1,U,Dic,A,B).
c_equal(var(_,N),U,_,Dic,Dic,A,B) :- !,
	cached_ref(N,N1,Dic),
	top_get(N1,U,Dic,A,B).
c_equal(list(L0,L1),list(L2,L3),Size,Dic0,Dic,A,B) :- !,
	c_equal(L0,L2,Size,Dic0,Dic1,A,C),
	c_equal(L1,L3,Size,Dic1,Dic,C,B).
c_equal(structure(S,S0),structure(S,S1),Size,Dic0,Dic,A,B) :- !,
	c_equal_args(S0,S1,Size,Dic0,Dic,A,B).
c_equal(_,_,_,Dic,Dic,A,B) :-
	'C'(A,fail,B),					%% An impossible match.
	barf(' =/2 cannot match.').


c_equal_args([],[],_,Dic,Dic,A,A).
c_equal_args([X|Xs],[Y|Ys],Size,Dic0,Dic,A,B) :-
	c_equal(X,Y,Size,Dic0,Dic1,A,C),
	c_equal_args(Xs,Ys,Size,Dic1,Dic,C,B).


%-----------------------
% Explicit unif. of
%   variable and
%   non-variable.
%
% Introduce 'T1' to 
% avoid a cache clash.
%-----------------------

top_get(V,X,Dic,A,B) :-
	put_value(V,T1,A,C),
	c_get_arg(X,T1,Dic,C,B).

top_put(V,X,_,Dic,Dic,A,B) :-
	put_variable(V,T1,A,C),			%% think about caching here
	c_get_arg(X,T1,Dic,C,B).


/*===================================================

  Treat explicit unif. of two variables as a special
  case, because it's faster and because of the
  dangling pointer problem: 'get_variable Yn,Xm'
  may place a dangling pointer in 'Yn'.

  ===================================================*/

c_eq_vars(U,V,Du,Dv,Dic,A,B) :-
	var(Du),
	var(Dv),!,
	c_eq_var_var(U,V,Du,Dv,Dic,A,B).
c_eq_vars(U,V,Du,Dv,Dic,A,B) :-
	var(Du),
	cached_ref(V,V1,Dic),!,
	c_eq_var_value(U,V1,Du,Dv,Dic,A,B).
c_eq_vars(U,V,Du,Dv,Dic,A,B) :-
	var(Dv),
	cached_ref(U,U1,Dic),!,
	c_eq_var_value(V,U1,Dv,Du,Dic,A,B).
c_eq_vars(U,V,_u,_v,Dic,A,B) :-
	cached_ref(U,U1,Dic),
	cached_ref(V,V1,Dic),
	c_eq_value_value(U1,V1,A,B).


c_eq_var_var('x'(I),V,D,D,_,A,B) :- !,
	put_variable(V,I,A,B),
	(V='y'(D); D=g).

c_eq_var_var('y'(I),V,I,Dv,Dic,A,B) :-
	'C'(A,put_y_variable(I,K),C),
	c_eq_var_value(V,'x'(K),Dv,I,Dic,C,B).


c_eq_var_value('x'(I),V,D,D,_,A,B) :- !,
	put_value(V,I,A,B).

c_eq_var_value('y'(I),'x'(T),g,g,Dic,A,B) :- !,
	cache_add(Dic,T1,var(g,'y'(I))),		%% using T loses here!
	'C'(A,put_x_value(T,T1),C),
	'C'(C,get_y_variable(I,T1),B).

c_eq_var_value('y'(I),V,I,_,_,A,B) :-
	'C'(A,put_y_variable(I,K),C),
	c_eq_value_value(V,'x'(K),C,B).


c_eq_value_value('x'(K),V,A,B) :- !,
	get_value(V,K,A,B).

c_eq_value_value(V,'x'(K),A,B) :- !,
	get_value(V,K,A,B).

c_eq_value_value(U,V,A,B) :- 
	put_value(U,K,A,C),
	get_value(V,K,C,B).


goal_args(goal(_,Size,structure(F,Args)),F/A,Size,Args) :-
	length(Args,A).

or_goal_args(goal(_,Size,G),F/A,Size,Args) :-
	structure(G,or_par(structure(F,Args))),
	length(Args,A).

cge_entry_point(goal(_,Env,structure(&,Args)),Size,EnvSize) :-
	number_of(Args,Size),
	maxY(Args,Env,Max),		
	EnvSize is Max+1.		
%% This is a REAL KLUDGE
%%  to figure the environment
%%  for the parcall frame
%%  BOO...HISSSSS
%%  (we should fix allocate_perms)

maxY([],X,X).
maxY([E|Es],Yi,Yo) :-
	maxY(E,Yi,Yj),
	maxY(Es,Yj,Yo).
maxY(structure(_,Args),Yi,Yo) :-
	maxY(Args,Yi,Yo).
maxY(var(_,y(N)),Yi,Ymax) :- nonvar(N),
	(  Yi > N,!,Ymax = Yi;Ymax= N).
maxY(_,X,X).

/*===================================================
   Compile the body up to first general call.
  ===================================================*/

c_guards([],N,_,A,B) :- !,
	A=[neck(N), proceed |B].

c_guards([G|Gs],N,Dic,A,B) :-
	inline_codable_goal(G,_),
	c_goal(G,Dic,Dic1,A,C),!,
	c_guards(Gs,N,Dic1,C,B).

c_guards(L,N,Dic,A,B) :-
	'C'(A,neck(N),C),
	c_goals(L,Dic,C,B).

%----------------------------
% Compile a tail of the body.
%----------------------------

c_goals([],_,[proceed|A],A).

c_goals([G],Dic,A,B) :-
 	c_last_goal(G,Dic,A,B),!.

c_goals([G|Gs],_,A,B) :-
	cge_entry_point(G,Size,Env),!,
	'C'(A,pcall(Size,Labels,Env),A0),
	'C'(A0,pop_wait,A1),
	c_goals(Gs,_,A1,A2),
	c_andpar_goals(G,_,Labels,A2,B).

c_goals([G|Gs],Dic,A,B) :-
 	c_goal(G,Dic,Dic1,A,C),!,
	c_goals(Gs,Dic1,C,B).

%----------------------------
% Compile a body goal.
%----------------------------

c_goal(G,Dic0,Dic,A,B) :-
	inline_codable_goal(G,Fu),
	open_code(Fu,Dic0,Dic,A,B).
c_goal(G,_,_,A,B) :-
	cge_entry_point(G,Size,Env),!,
	'C'(A,pcall(Size,Labels,Env),C),
	'C'(C,pop_wait,D),
	c_andpar_goals(G,_,Labels,D,B).
c_goal(G,Dic,_,A,B) :-
	or_goal_args(G,Fu,Size,Args),
	c_goal_args(Args,Size,Dic,_,A,C), 
	'C'(C,call_or(Fu,Size),B).
c_goal(G,Dic,_,A,B) :-
	goal_args(G,Fu,Size,Args),
	c_goal_args(Args,Size,Dic,_,A,C), 
	'C'(C,call(Fu,Size),B).

%----------------------------
% Compile last body goal.
%----------------------------

c_last_goal(G,_,A,B) :-
	cge_entry_point(G,Size,Env),!,
	'C'(A,pcall(Size,Labels,Env),A0),
	'C'(A0,pop_wait,A1),
	'C'(A1,proceed,A2),
	c_andpar_goals(G,_,Labels,A2,B).
c_last_goal(G,Dic,A,B) :-
	inline_codable_goal(G,Fu),
	open_code(Fu,Dic,_,A,C),
	'C'(C,proceed,B).
c_last_goal(G,Dic,A,B) :-
	or_goal_args(G,Fu,Size,Args),
	c_goal_args(Args,Size,Dic,_,A,C),
	'C'(C,execute_or(Fu),B).
c_last_goal(G,Dic,A,B) :-
	goal_args(G,Fu,Size,Args),
	c_goal_args(Args,Size,Dic,_,A,C), 
	'C'(C,execute(Fu),B).

%----------------------------
% Compile AND parallel goals
%----------------------------

%c_andpar_goals(goal(_,Env,PARGOALS),Dic,Label)
c_andpar_goals(goal(_,Env,PARGOALS),Dic,RevLab,A,B) :-
	'C'(A,parallel_start,C),
	c_and_args(PARGOALS,Env,Dic,Label,_,C,D),
	rev(Label,[],RevLab),
	'C'(D,parallel_end,B).

rev([],X,X).
rev([H|T],L,Rev) :- rev(T,[H|L],Rev).

/*===================================================
  Head arguments unification.  

  Ignore void args,  then unify args that
  are temp vas, finally unify perm vas and
  structures.  This seems to need fewer temporaries
  than the source sequence.  Doing perm vas earlier
  would need allocate earlier.

  ===================================================*/

c_head_args(structure(_,Args),Dic,N,A,B) :-
	head_arguments(Args,0,N,Arga-Argb,Argb-[],[]),
	c_head_args_c(Arga,Dic,A,B).

c_head_args_c([],_,A,A).
c_head_args_c([X=Y|Xs],Dic,A,B) :-
	c_get_arg(X,Y,Dic,A,C),!,
	c_head_args_c(Xs,Dic,C,B).


head_arguments([],N,N,Xs-Xs,Ys-Ys,_).
head_arguments([A|As],I,N,[A=I|X]-Xs,Ys,Seen) :-
	A = var(_,_),
	nocontainsx(Seen,A),
	I1 is I+1,!,
	head_arguments(As,I1,N,X-Xs,Ys,[A|Seen]).
head_arguments([A|As],I,N,Xs,[A=I|Y]-Ys,Seen) :-
	I1 is I+1,!,
	head_arguments(As,I1,N,Xs,Y-Ys,Seen).

/*===================================================
  Goal arguments unification.

  First put args that  are structures.  This seems
  to need fewer temporaries than the source sequence.

  ===================================================*/

c_goal_args(Xs,Size,Dic0,Dic,A,B) :-
	goal_arguments(Xs,0,Argb-[],Arga-Argb),
	c_goal_args_c(Arga,Size,Dic0,Dic,A,B).

c_goal_args_c([],_,D,D,A,A).
c_goal_args_c([X=Y|Xs],Size,Dic0,Dic,A,B) :-
	c_put_arg(X,Y,Size,Dic0,Dic1,A,C),!,
	c_goal_args_c(Xs,Size,Dic1,Dic,C,B).


goal_arguments([],_,Ys-Ys,Zs-Zs).
goal_arguments([Arg|Args],I,[Arg=I|Y]-Ys,Zs) :-
	Arg=var(_,_),
	I1 is I+1,!,
	goal_arguments(Args,I1,Y-Ys,Zs).
goal_arguments([Arg|Args],I,Ys,[Arg=I|Z]-Zs) :-
	I1 is I+1,!,
	goal_arguments(Args,I1,Ys,Z-Zs).

c_and_args(structure(&,PGoals),Env,Dic,Labs,Off,A,B) :-
	c_and_args(PGoals,Env,Dic,Labs,Off,A,B).
c_and_args(structure(Fu,Args),Env,Dic,[branch(Off,La)],Off,A0,B) :-
	'C'(A0,label(La),A1),
	c_goal_args(Args,Env,Dic,_,A1,A2),
	length(Args,A),
	'C'(A2,execute(Fu/A),B).
c_and_args([],_,_,[],_,A,A).
c_and_args([G|Gs],Env,Dic,Labels,Off,A,B) :-
	c_and_args(G,Env,Dic,Lab0,Off,A,C),
	c_and_args(Gs,Env,Dic,Lab1,Off,C,B),
	dlist(Lab0,Labels,Lab1).

%----------------------
%  Linearize term
%  before emitting
% 'get'+'put'+'unify'.
%---------------------

flat1(list(_,_),put,g).
flat1(list(_,_),get,_).
flat1(structure(_,_),put,g).
flat1(structure(_,_),get,_).

flat(S,V,put,A,B) :- !,
	flat_last(S,S1,put,A,C),
	'C'(C,V=S1,B).
flat(S,V,get,A,B) :- 
	'C'(A,V=S1,C),
	flat_last(S,S1,get,C,B).

flat_nolast(S,var(D,'x'(T)),GP,A,B) :-
	flat1(S,GP,D), !,
	flat(S,T,GP,A,B).
flat_nolast(S,S,_,A,A).

flat_last(list(A,B),list(A1,B1),GP,T,T0) :- !,
	flat_nolast(A,A1,GP,T,T1),
	flat_last(B,B1,GP,T1,T0).
flat_last(structure(S,L),structure(S,L1),GP,T,T0) :- !,
	flat_args(L,L1,GP,T,T0).
flat_last(S,S,_,T,T).


flat_args([],[],_,A,A).
flat_args([X],[X1],GP,A,B) :- !,
	flat_last(X,X1,GP,A,B).
flat_args([X|Xs],[X1|Xs1],GP,A,B) :-
	flat_nolast(X,X1,GP,A,C),
	flat_args(Xs,Xs1,GP,C,B).

%
% Compile matching a head argument.
% First linearize goal argument.
%

c_get_arg(X,V,Dic,A,B) :-
	flat(X,V,get,S,[]),
	c_get(S,Dic,A,B).

c_get([],_,A,A).				%% Using X registers as a cache
c_get([V=X|R],Dic,A,B) :-			%%  for Y values and constants.
	c_get(X,V,Dic,A,C),!,
	c_get(R,Dic,C,B).

:- mode c_get(+,?,?,?,?).

c_get(var(D,N),V,Dic,A,B) :-
	var(D), D=r,
	get_variable(N,V,A,B),!,
	copy_status(D,D1),
	cache_add(Dic,V,var(D1,N)).

c_get(var(D,N),V,Dic,A,B) :-
	( cache_search_var(Dic,D1,N1,N); D=D1, N=N1),
	get_value(N1,V,A,B),!,
	cache_add(Dic,V,var(D1,N)).

c_get(constant(K),V,Dic,A,B) :-
	'C'(A,get_constant(K,V),B),!,
	cache_add(Dic,V,constant(K)).

c_get(nil,V,Dic,A,B) :-
	'C'(A,get_nil(V),B),!,
	cache_add(Dic,V,nil).

c_get(list(X,Y),V,Dic,A,B) :-
	'C'(A,get_list(V),A1),
	c_unify(X,Dic,A1,A2),
	c_unify(Y,Dic,A2,B).

c_get(structure(F,Args),V,Dic,A0,B) :-
	length(Args,A), 
	'C'(A0,get_structure(F/A,V),A1),
	c_unify_args(Args,Dic,A1,B).

%
% Compile loading a goal argument.
% First linearize goal argument.
%

c_put_arg(X,V,Size,Dic0,Dic,A,B) :-
	flat(X,V,put,S,[]),
	c_put(S,Size,Dic0,Dic,A,B).

c_put([],_,Dic,Dic,A,A).
c_put([V=X|R],Size,Dic0,Dic,A,B) :-
	c_put(X,V,Size,Dic0,Dic1,A,C),!,
	c_put(R,Size,Dic1,Dic,C,B).

:- mode c_put(+,?,?,?,?,?,?).

c_put(var(D,N),V,_,Dic,Dic1,A,B) :-
	var(D), 
	( N='y'(I), D=I; D=g ),
	put_variable(N,V,A,B),!,
	copy_status(D,D1),
	cache_store(Dic,V,var(D1,N),Dic1).

c_put(var(D,N),V,Size,Dic,Dic1,A,B) :-
	( cache_search_var(Dic,D1,N1,N),
	  emit_put_value(D1,Size,N1,V,A,B),
	  cache_store_nonvar(Dic,V,var(D1,N),Dic1)
        ;
	  copy_status(D,D1),
	  emit_put_value(D1,Size,N,V,A,B),
	  cache_store(Dic,V,var(D1,N),Dic1)
        ),!.

c_put(constant(K),V,_,Dic,Dic1,A,B) :-
	( cache_search(Dic,N1,constant(K)), 
	  cache_store_nonvar(Dic,V,constant(K),Dic1),
	  'C'(A,put_x_value(N1,V),B)
        ;
	  cache_store(Dic,V,constant(K),Dic1),
	  'C'(A,put_constant(K,V),B)
        ),!.

c_put(nil,V,_,Dic,Dic1,A,B) :-
	( cache_search(Dic,N1,nil), 
	  cache_store_nonvar(Dic,V,nil,Dic1),
	  'C'(A,put_x_value(N1,V),B)
        ;
	  cache_store(Dic,V,nil,Dic1),
	  'C'(A,put_nil(V),B)
        ),!.

c_put(list(X,Y),V,_,Dic,Dic1,A,B) :-
	'C'(A,put_list(V),C),
	cache_clear(Dic,V,Dic1), 
	c_unify(X,Dic1,C,D),
	c_unify(Y,Dic1,D,B).

c_put(structure(F,Args),V,_,Dic,Dic1,A0,B) :-
	length(Args,A),
	'C'(A0,put_structure(F/A,V),C), 
	cache_clear(Dic,V,Dic1),
	c_unify_args(Args,Dic1,C,B).

c_unify_args([],_,A,A).
c_unify_args([X|Xs],Dic,A,B) :-
	c_unify(X,Dic,A,C),
	c_unify_args(Xs,Dic,C,B).

:- mode c_unify(+,?,?,?).

c_unify(var(D,N),_,A,B) :-
	var(D),
	D=g,!,
	unify_variable(N,A,B).

c_unify(var(D,N),Dic,A,B) :-
	( cache_search_var(Dic,D1,N1,N)
	; N=N1,
	  copy_status(D,D1)
	),!,
	emit_unify_value(D1,N1,A,B).

c_unify(constant(K),_,[unify_constant(K)|A],A).

c_unify(nil,_,[unify_nil|A],A).

c_unify(list(X,Y),Dic,A,B) :-
	'C'(A,unify_list,C),
	c_unify(X,Dic,C,D),
	c_unify(Y,Dic,D,B).

c_unify(structure(F,Args),Dic,A0,B) :-
	length(Args,A),
	'C'(A0,unify_structure(F/A),A1),
	c_unify_args(Args,Dic,A1,B).

						%%
cache_add(C,Key,Item) :- dic_lookup(C,Key,Item). %% Use argument registers
						%% as a cache for constants
cache_search(D,K,Val) :-			%% and variables.
	dic_node(D,dic(K,Val0,_,_)),		%% A cache is a binary tree.
	Val==Val0.				%%

cache_search_var(D,Desc,'x'(K),Val) :-
	dic_node(D,dic(K,var(Desc,Val0),_,_)),
	Val==Val0.
						%%
cache_store(C,Key,Item,C1) :-			%% Avoid caching X vars in
	dic_replace(C,Key,Item,C1).		%% random X vars.
						%%

cache_store_nonvar(C,Key,_,C) :- var(Key), !.
cache_store_nonvar(C,Key,Item,C1) :- dic_replace(C,Key,Item,C1).
cache_clear(C,Key,C1) :- dic_replace(C,Key,_,C1).

:- mode copy_status(+,?).

copy_status(g,g) :- !.
copy_status(r,d(_,r,_)) :- !.
copy_status(I,d(I,_,_)) :- integer(I).


emit_put_value(d(I,R,_),Size,N,V,A,B) :-
	var(R), 
	I>=Size, 
	R=r,!,
	put_unsafe_value(N,V,A,B).
emit_put_value(_,_,N,V,A,B) :-
	put_value(N,V,A,B).

emit_unify_value(d(_,_,G),N,A,B) :-
	var(G),                 		%% shallow backtracking
/*	R=r, G=g, */
	!,
	unify_local_value(N,A,B).
emit_unify_value(_,N,A,B) :-
	unify_value(N,A,B).


/*===================================================
  Lifetime analysis and allocation of X registers.

  Essentially backward traversal of a chunk,
  maintaining list of live X's.  Allocate new X's
  as encountered, using various heuristics.
  Until first call, take head into account.

  ===================================================*/

guard_lifetime(Live,A,B) :-
	'C'(A,I,A0),
	general_lifetime(I,Live,Liveall,Eqs,A0,A1),
	single_eqs_alloc(Eqs),
	temp_alloc(Liveall),
    	!,
	body_lifetime(_,A1,B).

						%% After first call, don't 
						%% take head into account.
body_lifetime(Live,A,A) :- !, Live=[].
body_lifetime([],A,B) :-
        'C'(A, parallel_end, C), !,
        body_lifetime(_, C, B).
body_lifetime(Live,A,B) :-
	'C'(A,J,A0),
	simple_lifetime_1(J,Live,A0,A1),!,
	body_lifetime(_,A1,B).

simple_lifetime_1(put_x_value(U,U),Live,A,B) :- !,
	'C'(A,I1,C),
	simple_lifetime_1(I1,Live,C,B).
simple_lifetime_1(I,Live,A0,B) :-
	xfer_insn(I,A), !,
	B=A0,
	arg_sequence(0,A,Live).
simple_lifetime_1(I,Live,A,B) :-
	'C'(A,I1,C),
	simple_lifetime_1(I1,Live1,C,B),
	x_def_use(I,DU), !,
	simple_x_alloc(DU,Live1,Live,I).

simple_x_alloc(x_d0u0,Set,Set,_).
simple_x_alloc(x_d0u1(A),Set0,Set,_) :-
	x_alloc(A,Set0,Set).

simple_x_alloc(x_d0u2(A,B),Set0,Set,_) :-
	x_alloc(A,Set0,Set1),
	x_alloc(B,Set1,Set).

simple_x_alloc(x_d0u3(A,B,C),Set0,Set,_) :-
	x_alloc(A,Set0,Set1),
	x_alloc(B,Set1,Set2),
	x_alloc(C,Set2,Set).

simple_x_alloc(x_d1u0(X),Set0,Set,I) :-
	delete_check(X,Set0,Set,I).

simple_x_alloc(x_d1u1(X,A),Set0,Set,I) :-
	delete_check(X,Set0,Set1,I),
	x_alloc(A,Set1,Set).

simple_x_alloc(x_d1u2(X,A,B),Set0,Set,I) :-
	delete_check(X,Set0,Set1,I),
	x_alloc(A,Set1,Set2),
	x_alloc(B,Set2,Set).

simple_x_alloc(x_d2u0(X,Y),Set0,Set,I) :-
	delete_check(X,Set0,Set1,I),
	delete_check(Y,Set1,Set,I).

delete_check(X,Set0,Set,_) :- set_delete(Set0,X,Set), !.
delete_check(_,Set,Set,I)  :- peepable_insn(I), !.
delete_check(X,Set0,Set,_) :- x_alloc(X,Set0,Set).

peepable_insn(get_x_variable(_,_)).		%% PHO will delete or replace
peepable_insn(put_x_variable(_,_)).		%% these if not fully 
peepable_insn(unify_x_variable(_)).		%% instantiated.

x_alloc(X,Set0,Set) :- 
	var(X),
	x_free_arg(Set0,0,X,Set).
x_alloc(X,Set0,Set) :- 
	nonvar(X),
	set_insert(Set0,X,Set).

x_free_arg([],I,I,[I]) :- !.
x_free_arg([X|Set0],I,I,[I,X|Set0]) :- X>I, !.
x_free_arg([X|Set0],I,Temp,[X|Set]) :- X<I, !, x_free_arg(Set0,I,Temp,Set).
x_free_arg(Set0,I,Temp,Set) :- I1 is I+1, x_free_arg(Set0,I1,Temp,Set).


general_lifetime(get_x_variable(X,A),Live,Dic,Eqs,A0,B) :-
	'C'(A0,I1,A1),
	general_lifetime(I1,Live1,Dic,Eqs0,A1,B),!,
	new_live_set_move(A,X,Live1,Live,Dic,Eqs,Eqs0).
general_lifetime(put_x_value(X,A),Live,Dic,Eqs,A0,B) :-
	'C'(A0,I1,A1),
	general_lifetime(I1,Live1,Dic,Eqs0,A1,B),!,
	new_live_set_move(X,A,Live1,Live,Dic,Eqs,Eqs0).
general_lifetime(I,Live,_,[],A0,B) :-
	xfer_insn(I,A),!,
	B=A0,
	arg_sequence(0,A,Live).
general_lifetime(I,Live,Dic,Eqs,A,B) :-
	'C'(A,I1,A1),
	general_lifetime(I1,Live1,Dic,Eqs,A1,B),
	x_def_use(I,DU),!,
	new_live_set(DU,Live1,Live,I,Dic).

new_live_set_move(From,To,Set0,Set,Dic,A,B) :-
	set_delete(Set0,To,Set1), 
	insert_ineq(From,Set1,Set,Dic),!,
	possible_eq(From,To,Dic,A,B).
new_live_set_move(_,_,Set,Set,_,A,A).

possible_eq(From,To,Dic,A,B) :-
	nonvar(From),
	var(To),
	dic_lookup(Dic,To,ToEntry),!,
	'C'(A,f(From,To,ToEntry),B).
possible_eq(From,To,Dic,A,B) :-
	var(From),
	nonvar(To),
	dic_lookup(Dic,From,FromEntry),!,
	'C'(A,f(To,From,FromEntry),B).
possible_eq(_,_,_,A,A).

new_live_set(x_d0u0,Set,Set,_,_).
new_live_set(x_d0u1(A),Set0,Set,_,Dic) :-
	insert_ineq(A,Set0,Set,Dic).
new_live_set(x_d0u2(A,B),Set0,Set,_,Dic) :-
	insert_ineq(A,Set0,Set1,Dic),
	insert_ineq(B,Set1,Set,Dic).
new_live_set(x_d0u3(A,B,C),Set0,Set,_,Dic) :-
	insert_ineq(A,Set0,Set1,Dic),
	insert_ineq(B,Set1,Set2,Dic),
	insert_ineq(C,Set2,Set,Dic).
new_live_set(x_d0un(0),Set,Set,_,_).
new_live_set(x_d0un(N),Set0,Set,I,Dic) :-
	N>0,
	N1 is N-1,
	insert_ineq(N1,Set0,Set1,Dic),
	new_live_set(x_d0un(N1),Set1,Set,I,Dic).
new_live_set(x_d1u0(X),Set0,Set,I,Dic) :-
	delete_check(X,Set0,Set,I,Dic).
new_live_set(x_d1u1(X,A),Set0,Set,I,Dic) :-
	delete_check(X,Set0,Set1,I,Dic),
	insert_ineq(A,Set1,Set,Dic).
new_live_set(x_d1u2(X,A,B),Set0,Set,I,Dic) :-
	delete_check(X,Set0,Set1,I,Dic),
	insert_ineq(A,Set1,Set2,Dic),
	insert_ineq(B,Set2,Set,Dic).
new_live_set(x_d2u0(X,Y),Set0,Set,I,Dic) :-
	delete_check(X,Set0,Set1,I,Dic),
	delete_check(Y,Set1,Set,I,Dic).

delete_check(X,Set0,Set,_,_)  :- set_delete(Set0,X,Set), !.
delete_check(_,Set,Set,I,_)   :- peepable_insn(I), !.
delete_check(X,Set,Set,_,Dic) :- all_ineq(X,Set,Dic).

all_ineq(X,Set,Dic) :-
	var(X),
	dic_lookup(Dic,X,XEntry),
	all_ineq_var(Set,X,XEntry,Dic)
    ;
	nonvar(X),
	all_ineq_nonvar(Set,X,Dic).

insert_ineq(X,Set0,Set,Dic) :-
	var(X),
	dic_lookup(Dic,X,XEntry),
	insert_ineq_var(X,XEntry,Set0,Set,Dic);
	nonvar(X),
	insert_ineq_nonvar(X,Set0,Set,Dic).

insert_ineq_var(A,_,[],[A],_) :- !.
insert_ineq_var(A,_,[D|Ds],[D|Ds],_) :- A==D, !.
insert_ineq_var(A,AEntry,[D|Ds],[A,D|Ds],Dic) :-
	A@<D,
	dif_var(D,A,AEntry,Dic),
	!, all_ineq_var(Ds,A,AEntry,Dic).
insert_ineq_var(A,AEntry,[D|Ds],[D|Es],Dic) :-
	dif_var(D,A,AEntry,Dic),
	insert_ineq_var(A,AEntry,Ds,Es,Dic).

insert_ineq_nonvar(A,[],[A],_) :- !.
insert_ineq_nonvar(A,[D|Ds],[D|Ds],_) :- A==D, !.
insert_ineq_nonvar(A,[D|Ds],[A,D|Ds],_) :- A@<D, !.
insert_ineq_nonvar(A,[D|Ds],[D|Es],Dic) :-
	dif_nonvar(D,A,Dic),
	insert_ineq_nonvar(A,Ds,Es,Dic).

all_ineq_var([],_,_,_).
all_ineq_var([D|Ds],A,AEntry,Dic) :-
	dif_var(D,A,AEntry,Dic),
	all_ineq_var(Ds,A,AEntry,Dic).

all_ineq_nonvar([],_,_).
all_ineq_nonvar([D|Ds],A,Dic) :-
	dif_nonvar(A,D,Dic),
	all_ineq_nonvar(Ds,A,Dic).

dif_var(D,A,AEntry,Dic) :-
	var(D),
	dic_lookup(Dic,D,DEntry),
	containsx(AEntry,D), containsx(DEntry,A);
	nonvar(D),
	containsx(AEntry,D).

dif_nonvar(D,A,Dic) :-
	var(D),
	dic_lookup(Dic,D,DEntry),
	containsx(DEntry,A);
	nonvar(D).


/*===================================================
  Allocation of temporaries.
  Try to get as many 'get_variable(U,U)' and
  'put_value(U,U)' as possible.
  ===================================================*/

single_eqs_alloc([]).
single_eqs_alloc([f(X,X,Entry)|Fs]) :-
	nocontainsx(Entry,X),!,
	single_eqs_alloc(Fs).
single_eqs_alloc([_|Fs]) :-
	single_eqs_alloc(Fs).

temp_alloc(Dic) :- var(Dic), !.
temp_alloc(dic(T,Neqs,L,R)) :-
	temp_alloc(L),
	(
	    nonvar(T)
	;   temp_free_arg(T,Neqs,0)
    	),!,
	temp_alloc(R).

temp_free_arg(I,Neqs,I) :- nocontainsx(Neqs,I).
temp_free_arg(T,Neqs,I) :- I1 is I+1, temp_free_arg(T,Neqs,I1).

/*===================================================
       END OF GENERAL REGISTER ALLOCATION
  ===================================================*/

% Convenient generic instructions.

cut('x'(I)) --> [cut_x(I)].
cut('y'(I)) --> [cut_y(I)].

dcut('x'(I)) --> [cut_x(I)].
dcut('y'(I)) --> [cut_y(I)].

put_variable('x'(I),J) --> [put_x_variable(I,J)].
put_variable('y'(I),J) --> [put_y_variable(I,J)].

put_value('x'(I),J) --> [put_x_value(I,J)].
put_value('y'(I),J) --> [put_y_value(I,J)].

put_unsafe_value('x'(I),J) --> [put_x_unsafe_value(I,J)].
put_unsafe_value('y'(I),J) --> [put_y_unsafe_value(I,J)].

get_variable('x'(I),J) --> [get_x_variable(I,J)].
get_variable('y'(I),J) --> [get_y_variable(I,J)].

get_value('x'(I),J) --> [get_x_value(I,J)].
get_value('y'(I),J) --> [get_y_value(I,J)].

unify_variable('x'(I)) --> [unify_x_variable(I)].
unify_variable('y'(I)) --> [unify_y_variable(I)].

unify_value('x'(I)) --> [unify_x_value(I)].
unify_value('y'(I)) --> [unify_y_value(I)].

unify_local_value('x'(I)) --> [unify_x_local_value(I)].
unify_local_value('y'(I)) --> [unify_y_local_value(I)].

% What X registers does an instruction use or define?

:- mode x_def_use(+,?).

x_def_use(neck(N),x_d0un(N)).
x_def_use(cut_x(A),x_d0u1(A)).
x_def_use(function_1(_,V,X),x_d1u1(V,X)).
x_def_use(function_2(_,V,X,Y),x_d1u2(V,X,Y)).
x_def_use(builtin_1(_,X),x_d0u1(X)).
x_def_use(builtin_2(_,X,Y),x_d0u2(X,Y)).
x_def_use(builtin_3(_,X,Y,Z),x_d0u3(X,Y,Z)).
x_def_use(get_x_variable(V,A),x_d1u1(V,A)).
x_def_use(get_y_variable(_,A),x_d0u1(A)).
x_def_use(get_x_value(V,A),x_d0u2(V,A)).
x_def_use(get_y_value(_,A),x_d0u1(A)).
x_def_use(get_constant(_,A),x_d0u1(A)).
x_def_use(get_nil(A),x_d0u1(A)).
x_def_use(get_structure(_,A),x_d0u1(A)).
x_def_use(get_list(A),x_d0u1(A)).
x_def_use(put_x_variable(V,A),x_d2u0(V,A)).
x_def_use(put_y_variable(_,A),x_d1u0(A)).
x_def_use(put_x_value(V,A),x_d1u1(A,V)).
x_def_use(put_y_value(_,A),x_d1u0(A)).
x_def_use(put_x_unsafe_value(V,A),x_d1u1(A,V)).
x_def_use(put_y_unsafe_value(_,A),x_d1u0(A)).
x_def_use(put_constant(_,A),x_d1u0(A)).
x_def_use(put_nil(A),x_d1u0(A)).
x_def_use(put_structure(_,A),x_d1u0(A)).
x_def_use(put_list(A),x_d1u0(A)).
x_def_use(unify_x_variable(V),x_d1u0(V)).
x_def_use(unify_x_value(V),x_d0u1(V)).
x_def_use(unify_x_local_value(V),x_d0u1(V)).
%% [MCL:BUILTIN_INC]
x_def_use(builtin_inc(X,Y),x_d0u2(X,Y)).
x_def_use(_,x_d0u0).

% What Y variables does an instruction define?

:- mode y_defining_some(+,?).


y_defining_some(get_y_variable(X,_),[X]).
y_defining_some(unify_y_variable(X),[X]).
y_defining_some(put_y_variable(X,_),[X]).


% What instructions jump?

:- mode xfer_insn(+,?).

xfer_insn(call(_/A,_),A).
xfer_insn(call_or(_/A,_),A).
xfer_insn(execute(_/A),A).
xfer_insn(execute_or(_/A),A).
xfer_insn(pop_wait,0).
xfer_insn(proceed,0).
%?xfer_insn(fail,0).

arg_sequence(I,I,[]) :- !.
arg_sequence(I,J,[I|Is]) :- I1 is I+1, arg_sequence(I1,J,Is).

% NO WARNINGS FOR NOW.
barf(_).

%-----------------------------------------------------------------------------
% Final pass over entire compiled code to emit 'allocate', 'deallocate',
% and cut related instructions.

noallocs([parallel_end|Insns],A,B) :- !,
	peep(Insns,A,B).
noallocs([Insn|Insns],A,B) :-
	peep_1(Insn,A,C),!,
	noallocs(Insns,C,B).

noallocs([parallel_end|Insns],Env,A,B) :- !,
	peep(Insns,Env,A,B).
noallocs([Insn|Insns],Env,A,B) :-
	peep_1(Insn,A,C),!,
	noallocs(Insns,Env,C,B).

peep_clause(yes,Code,A,B) :- peep(Code,noenv,A,B).
peep_clause(no,Code,A,B) :- peep(Code,A,B).

% simple case: no environments.
peep([],A,A).
peep([parallel_start|Parcalls],A,B) :- !,
	noallocs(Parcalls,A,B).
peep([Insn|Insns],A,B) :-
	peep_1(Insn,A,C),!,
	peep(Insns,C,B).

:- mode peep_1(+,?,?).

peep_1(cut_x(-1),[cutb|A],A).
peep_1(cut_x(X),[cutb_x(X)|A],A).
peep_1(get_x_variable(U,U),A,A).
peep_1(put_x_value(U,U),A,A).
peep_1(put_x_value(-1,U),[choice_x(U)|A],A).
peep_1(put_x_variable(void,void),A,A).
peep_1(put_x_variable(A,A),[put_x_void(A)|B],B).
peep_1(unify_x_variable(void),[unify_void|A],A).
peep_1(goto(X),[execute(X)|A],A).
peep_1(Insn,[Insn|A],A).

% general case: treat environments.
peep([],_,A,A).
peep([parallel_start|Parcalls],Env,A,B) :- !,
	noallocs(Parcalls,Env,A,B).
peep([Insn|Insns],Env,A,B) :-
	peep_1(Insn,Env,Env1,A,C),!,
	peep(Insns,Env1,C,B).

:- mode peep_1(+,?,?,?,?).

peep_1(else,_,noenv,[else|L],L).
peep_1(cut_x(-1),noenv,noenv,[cutb|L],L).
peep_1(cut_x(X),noenv,noenv,[cutb_x(X)|L],L).
peep_1(cut_x(-1),env(A,B),env(A,B),[cute|L],L).
peep_1(cut_x(X),env(A,B),env(A,B),[cute_x(X)|L],L).
peep_1(call(U,A),Env,Env1,L,L0) :-
	ensure_call(Env,Env1,A,L,[call(U,A)|L0]).
peep_1(pcall(U,L,A),Env,Env1,L0,L2) :-
	ensure_init(U,Env,E,L0,L1),
	ensure_call(E,Env1,A,L1,[pcall(U,L,A)|L2]).
peep_1(call_or(U,A),Env,Env1,L,L0) :-
	ensure_call(Env,Env1,A,L,[call_or(U,A)|L0]).
peep_1(execute(U),Env,Env1,L,L0) :-
	ensure_no_env(Env,Env1,L,[execute(U)|L0]).
peep_1(execute_or(U),Env,Env1,L,L0) :-
	ensure_no_env(Env,Env1,L,[execute_or(U)|L0]).
peep_1(proceed,Env,Env1,L,L0) :-
	ensure_no_env(Env,Env1,L,[proceed|L0]).
peep_1(get_x_variable(U,U),Env,Env,L,L).
peep_1(get_y_variable(U,-1),Env,Env1,L,L0) :-
	ensure_env(Env,Env1,[U],L,[choice_y(U)|L0]).
peep_1(put_x_value(U,U),Env,Env,L,L).
peep_1(put_x_value(-1,U),Env,Env,[choice_x(U)|L],L).
peep_1(put_x_variable(void,void),Env,Env,L,L).
peep_1(put_x_variable(A,A),Env,Env,[put_x_void(A)|L],L).
peep_1(get_y_variable(Y,A),env(E),env(E),[get_y_first_value(Y,A)|L],L).
peep_1(put_y_variable(Y,A),env(E),env(E),[put_y_value(Y,A)|L],L).
peep_1(unify_y_variable(Y),env(E),env(E),[unify_y_first_value(Y)|L],L).
peep_1(unify_x_variable(void),Env,Env,[unify_void|L],L).
peep_1(Insn,Env,Env1,L,L0) :-
	y_defining_some(Insn,Ys),
	ensure_env(Env,Env1,Ys,L,[Insn|L0]).
peep_1(goto(X),Env,Env,[execute(X)|L],L).
peep_1(Insn,Env,Env,[Insn|L],L).

ensure_env(noenv,env(_,Ys),Ys,[allocate|L],L).
ensure_env(env(Size),env(Size),_,L,L).
ensure_env(env(Size,Ys),env(Size,Ys1),[Y],L,L) :-
	set_insert(Ys,Y,Ys1).

ensure_call(noenv,env(_),Size,A,B) :-
	numberof_parcall_slots(N),
	'C'(A,allocate,C),
	init_list(0,Size,[],L),
	length(L,M),
	( M < N, L1 = L
	; strip_list(L,N,L1)
	),
	'C'(C,init(L1),B).
ensure_call(env(Size),env(_),Size,A,A).
ensure_call(env(Size,Ys),env(_),Size,A,B) :-
	numberof_parcall_slots(N),
	init_list(0,Size,Ys,L),
	length(L,M),
	( M < N, L1 = L
	; strip_list(L,N,L1)
	),
	'C'(A,init(L1),B).

ensure_no_env(noenv,noenv,A,A).
ensure_no_env(env(-1),noenv,[deallocate|A],A).

init_list(Max,Max,_,[]) :- !.
init_list(Y,Max,Perms,Ys) :-
   set_in(Y,Perms),
   Y1 is Y+1,
   !, init_list(Y1,Max,Perms,Ys).
init_list(Y,Max,Perms,[Y|Ys]) :-
   Y1 is Y+1,
   init_list(Y1,Max,Perms,Ys).

ensure_init(U,noenv,env(_,Env),[allocate|L],L) :- !,
	add_initial(U,Env).
ensure_init(U,env(X,L),env(X,Env),L,L) :-
	add_initial(U,L1),
	merge(L,L1,Env).
ensure_init(_,X,X,L,L).

add_initial(U,L1) :-
	empty_parcall_size(E),
	N is E+U,
	init_list(0,N,[],L1).

%-----------------------------------------------------------------------------

pl_pwam_in_progress :-
	recorded_internal(compile_file_emit,
	                  '$emit'(pwam_compile_file_emit(_)),_).

pl_ql_in_progress :-
	recorded_internal(compile_file_emit,
	                  '$emit'(ql_compile_file_emit(_)),_).

/*===================================================

  Directives affecting the parser are handled here.
  ALL DIRECTIVES ARE PASSED ON.

  ===================================================*/

:- mode do_at_eval_load_time(+).

do_at_eval_load_time(   (parallel _) ) :- !.
do_at_eval_load_time(   (mode _)     ) :- !.
do_at_eval_load_time(   (dynamic L)  ) :- !, compile_file_emit((dynamic L)).
do_at_eval_load_time(   (public L)   ) :- !, compile_file_emit((public  L)).
do_at_eval_load_time(    Goal        ) :-    compile_file_emit( call(Goal)).

:- mode do_at_compile_time(+).

do_at_compile_time(op(X,Y,Z)) :-
	(
	    pl_pwam_in_progress -> ensure_op_undone(Z,X,Y)
	;   pl_ql_in_progress  -> ensure_op_undone(Z,X,Y)
        ;   true
        ),
	op(X,Y,Z).

do_at_compile_time(  (parallel Spec) ) :- !, declare_parallel(Spec).
do_at_compile_time(  (mode Spec)     ) :- !, declare_mode(Spec).
do_at_compile_time(  (dynamic Spec)  ) :- !, declare_dynamic(Spec).
do_at_compile_time(_).

ensure_op_undone([Op|Ops],X,Y) :-
	ensure_op_undone(Op,X,Y),
	ensure_op_undone(Ops,X,Y).
ensure_op_undone(Op,X,Y) :-
	atom(Op),
	\+current_op(X,Y,Op) ->
	recorda_internal(Op,'$eraseop'(op(0,Y,Op)),_);
	true.

%---------------------
%  Declaring types
%---------------------

declare_parallel((S1,S2)) :- !,
	declare_parallel(S1),
	declare_parallel(S2).
declare_parallel(Pred) :-
	get_key(Pred,F),
	recorda_internal(F,'$parallel_decl'(Pred),_).

declare_dynamic((S1,S2)) :- !,
	declare_dynamic(S1),
	declare_dynamic(S2).
declare_dynamic(Pred) :-
	get_key(Pred,F),
	recorda_internal(F,'$dynamic_decl'(Pred),_).

declare_mode((S1,S2)) :- !,
	declare_mode(S1),
	declare_mode(S2).
declare_mode(Spec) :-
	functor(Spec,F,A),
	recorda_internal(Spec,'$mode_decl'(F/A,Spec),_).

%---------------------
%   Link individual
%   clauses so they
%   form a predicate
%   definition...
%---------------------

link_unit(Pred) :-
	index_predicate(Pred,Code,[]),
	compile_file_emit(predicate(Pred,Code)),
	!.

get_key(N/Ar,Key) :- get_key(N,Ar,Key).

get_key(N/Ar-_,_,Key) :- !, get_key(N,Ar,Key).
get_key(N,Ar,Key) :- functor(Key,N,Ar).

get_clause_id(structure(Name,Args),Name/Ar/I1,F) :-
   length(Args,Ar),
   get_key(Name,Ar,F),
   get_internal_id(Name,Ar,F),
   (recorded_internal(F,'$compiled'(Name/Ar/I,_,_),_), I1 is I+1; I1=1),
   !.

get_internal_id(Pred-S,Ar,F) :-
   var(S), 
   (recorded_internal(F,'$compiled'((Pred-S0)/Ar/_,_,_),_), S is S0+1; S=1),
   !.
get_internal_id(_,_,_).


:- mode mode_filter(+,?).

mode_filter(!,2'11110).
mode_filter(+,2'11110).
mode_filter(-,2'1).
mode_filter(?,2'11111).

pred_filter(F,Pred,T) :-
	recorded_internal(F,'$pred_filter'(Pred,T),_).
pred_filter(_,_,2'11111).

set_pred_filter(F,Pred,NewFilter,nohash,cut,OldFilter) :-
	Filter is OldFilter/\ \(NewFilter),
	recorda_internal(F,'$pred_filter'(Pred,Filter),_).
set_pred_filter(_,_,_,_,_,_).

record_internal(Pred/_,F) :-
	recorded_internal(F,'$predicate'(Pred),_), !;
	parallel_declaration(F,Pred,Flag),
	mode_declaration(F,Pred,Mode,_),
	compile_file_emit(declare_predicate(Pred,Mode,Flag)),
	recorda_internal(F,'$predicate'(Pred),_).

parallel_declaration(F,Pred,1) :-
	recorded_internal(F,'$parallel_decl'(Pred),_), !.
parallel_declaration(_,_,0).

dynamic_declaration(F,Pred) :-
	recorded_internal(F,'$dynamic_decl'(Pred),_), !.

mode_declaration(F,Pred,Mode,M) :-
	recorded_internal(F,'$mode_decl'(Pred,Mode),_),
	F=Mode,
	arg(1,Mode,M),
	!.
mode_declaration(_,_/Ar,Mode,M) :-
	functor(Mode,(mode),Ar),
	unspecific_mode(1,Mode),
	arg(1,Mode,M),
	!.
mode_declaration(_,_,(mode),?).

unspecific_mode(I,Mode) :-
	arg(I,Mode,?), !, I1 is I+1, unspecific_mode(I1,Mode).
unspecific_mode(_,_).

:- mode pwam_compile_file_emit(+).

pwam_compile_file_emit(clause(Pred,Code,_,_)) :-
	mock_format([n, d('clause('), q(Pred), d(',')]),
	transform_pwam(Code,Code1),
	write_code(Code1,'   ['),
	mock_format([d(']).'), n]).
pwam_compile_file_emit(declare_predicate(_,_,_)). %ignore
pwam_compile_file_emit(predicate(Pred,Code)) :-
	mock_format([n, d('predicate('), q(Pred), d(',')]),
	write_code(Code,'   ['),
	mock_format([d(']).'), n]).
pwam_compile_file_emit(call(G)) :-
	numbervars(G,0,_),
	mock_format([n, q(call(G)), d('.'), n]).
pwam_compile_file_emit((dynamic L)) :-
	mock_format([n, q((dynamic L)), d('.'), n]).
pwam_compile_file_emit((mode L)) :-
	mock_format([n, q((mode L)), d('.'), n]).

write_code([],_).
write_code([Insn|Insns],Del) :-
	write_insn(Insn,Del), !, write_code(Insns,'   ,').

write_insn(Insn,Del) :-
	nl, write(Del), writeq(Insn).

%-----------------------------------------------------------------------------

safe_structure(structure(F,Args),S) :- atom(F), S=..[F|Args].
structure(structure(F,Args),S) :- S=..[F|Args].

inner_cut('$cut'(X),'$dcut'(X)) :- !.
inner_cut(X,X).

%-----------------------------------------------------------------------------
% Binary tree access.

dic_node(Dic,Node) :- var(Dic), !, fail; Node=Dic.
dic_node(dic(_,_,L,R),Node) :- dic_node(L,Node); dic_node(R,Node).
 
dic_lookup(Dic,Key,Val) :-
	var(Dic), !, Dic=dic(Key,Val,_,_);
	Dic=dic(K,V,L,R),
	compare(X,Key,K),
	dic_lookup_c(X,Key,Val,V,L,R).
 
:- mode dic_lookup_c(+,?,?,?,?,?).
dic_lookup_c((=),_,Val,Val,_,_).
dic_lookup_c((<),Key,Val,_,L,_) :- dic_lookup(L,Key,Val).
dic_lookup_c((>),Key,Val,_,_,R) :- dic_lookup(R,Key,Val).

dic_replace(Dic,Key,Val,Dic1) :- var(Dic), !, Dic1=dic(Key,Val,_,_).
dic_replace(dic(Key1,Val1,L1,R1),Key,Val,dic(Key1,Val2,L2,R2)) :-
   (Key==Key1, Val2=Val, L2=L1, R2=R1;
    Key@<Key1, Val2=Val1, dic_replace(L1,Key,Val,L2), R2=R1;
    Key@>Key1, Val2=Val1, L2=L1, dic_replace(R1,Key,Val,R2)).

%-----------------------------------------------------------------------------
% General utility predicates.

% contains/2 has been replaced by member/2 in miscellaneous.pl and
% dlist/2 and merge/3 have been moved also to that file  (PBC 1/91)

c(S0,S,S0,S).

set_insert([],A,[A]) :- !.
set_insert([D|Ds],A,[A,D|Ds]) :- A@<D, !.
set_insert([D|Ds],A,[D|Ds]) :- A==D, !.
set_insert([D|Ds],A,[D|Bs]) :- set_insert(Ds,A,Bs).

set_delete([D|Ds],A,Ds) :- A==D, !.
set_delete([D|Ds],A,[D|Ds1]) :- A@>D, set_delete(Ds,A,Ds1).

nonsingle(X) :- X=[_], !, fail; true.

% Set membership.
set_in(O,[O1|Os]) :- O1@<O, !, set_in(O,Os); O==O1.

% Set membership, impure list.
set_in_ro(O,[O1|Os]) :- O1@<O, nonvar(Os), !, set_in_ro(O,Os); O==O1.

% Identity-membership.
containsx(Vals,Y) :- var(Vals), !, Vals=[Y|_].
containsx([X|_],Y) :- X==Y, !.
containsx([_|Xs],Y) :- containsx(Xs,Y).

% nocontainsx(L,X) if X not identical to any in L.
nocontainsx([],_).
nocontainsx([X1|Xs],X) :- X\==X1, nocontainsx(Xs,X).

first([X|_]) --> [X].

strip_list(L,0,L).
strip_list([_|T],N,L1) :- N1 is N-1, strip_list(T,N1,L1).

% -------------------------------------
% Utilities which used to live in other files.

:- mode structure_copy(+,?,?).
structure_copy(list(X,Y),list(X1,Y1),Dic) :-
   structure_copy(X,X1,Dic), structure_copy(Y,Y1,Dic).
structure_copy(structure(S,Args),structure(S,Args1),Dic) :-
   structure_copy_args(Args,Args1,Dic).
structure_copy(nil,nil,_).
structure_copy(constant(K),constant(K),_).
structure_copy(X,Y,Dic) :-
   X=var(_,_), dic_lookup(Dic,X,Y), Y=var(_,_).

structure_copy_args([],[],_).
structure_copy_args([X|Xs],[Y|Ys],Dic) :-
   structure_copy(X,Y,Dic), structure_copy_args(Xs,Ys,Dic).

%---------------------------------------------------------------------
%  This part is from the file "~muthu/warren/transform_wam.pl"
%---------------------------------------------------------------------

transform_pwam(X,Z) :-
	parse_pwam(Y,X,[]),
	convert_pwam(Y,Z),
	!.

%-----------------------------------------------

parse_pwam([X|Z],A,B) :-
	non_parallel_pwam_code(P,A,A0),
	( 'C'(A0,call('$check_me_else'/0,_),A1),
	  collect_code_till_label2(R,S,A1,A2),
	  Y = [P,check_me_else|R],
	  ( S = label2,
	    X =.. [parcall,Y],
	    parse_pwam(Z,A2,B)
	  ;
	    /* S = no_label2 */
	    X =.. [last_goal_parcall,Y],
	    Z = []
	  )
        ;
	  X =.. [no_parcall,P],
	  Z = []
        ),!.

%------------------------------------------------

non_parallel_pwam_code([X|Y],[X|L],L0) :-
	\+ X = call('$check_me_else'/0,_),
	non_parallel_pwam_code(Y,L,L0).
non_parallel_pwam_code([],L,L).

%------------------------------------------------

collect_code_till_label2(X,Y,A,B) :-
	parallel_code(P,A,A0),
	(  'C'(A0,'$label1_no_label2',A1),
	   Y = no_label2,
	   any_code(Q,A1,B)
        ;
	   'C'(A0,'$label1',A1),
	   Y = label2,
	   sequential_code(Q,A1,A2),
	   'C'(A2,'$label2',B)
        ),
	X = [P,label1,Q].

%-------------------------------------------------

parallel_code([X|Y],[X|L],L0) :-
	X \== '$label1',
	X \== '$label1_no_label2',
	parallel_code(Y,L,L0).
parallel_code([],L,L).

%-------------------------------------------------

sequential_code([X|Y],[X|L],L0) :-
	X \== '$label2',
	sequential_code(Y,L,L0).
sequential_code([],L,L).

%-------------------------------------------------

any_code([X1|Y1],[X|L],L0) :-
	X = execute(Functor/A),
	name(Functor,Fu),
	(  append("$pure_",F,Fu),
	   name(F1,F),
	   A1 is A-1,
	   X1 = pexecute_pure(F1/A1)
	;  append("$soft_",F,Fu),
	   name(F1,F),
	   A1 is A-2,
	   X1 = pexecute_soft(F1/A1)
	;  append("$hard_",F,Fu),
	   name(F1,F),
	   A1 is A-2,
	   X1 = pexecute_hard(F1/A1)
	;  X1 = X
	),
	any_code(Y1,L,L0).
any_code([X1|Y1],[X1|L],L0) :-
	any_code(Y1,L,L0).
any_code([],L,L).

%---------------------------------------------------

convert_pwam([P|Q],Z) :-
	add_labels(P,X),
	convert_pwam(Q,Y),
	append(X,Y,Z).
convert_pwam([],[]).

%----------------------------------------------------------------

add_labels(no_parcall(X),X).
add_labels(parcall([X,check_me_else,Y,label1,Z]),P) :-
	append(Z,[label(A)],Temp1),
	append([branch(A),label(B)],Temp1,Temp2),
	append(Y,Temp2,Temp3),
	append([check_me_else(B)],Temp3,Temp4),
	append(X,Temp4,P).
add_labels(last_goal_parcall([X,check_me_else,Y,label1,Z]),P) :-
	append([deallocate,proceed,label(A)],Z,Temp1),
	append(Y,Temp1,Temp2),
	append([check_me_else(A)],Temp2,Temp3),
	append(X,Temp3,P).

%----------------------------------------------------------------
%-----------------------------------------------------------------------------
%                              TABLES
%-----------------------------------------------------------------------------
% The following predicates are compiled using special instructions.

%% Moved to builtins_pwam.pl:    (PBC)
%%       inline_codable/1
%%       name_of_builtin/3 
%%       eval_builtin/1 
%%       name_of_function/3
