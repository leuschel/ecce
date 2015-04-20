:- module(type_exp,
	[ expand_types/2 ],
	[ assertions, 'types/type_ops', isomodes ]).

:- include(library('types/typedef_to_pred')).
:- include(library('types/pred_to_typedef')).
% Commented by PLG 2 April
% :- include(library('types/regtypes_lattice')).
:- include(library('types/fd_reg_type_lattice')).

:- comment(title,"Type definition support").

:- data found_type_decl/1.
:- data type_clauses/1.

expand_types((:- Dir), DeclAndCode) :-
        is_typedef(Dir,Type,Def,S),
	( valid_regular_type_rule_e(Dir,Error),
	  var(Error),
	  typedef_to_pred(Def, Type, Clauses),
	  norm_goal_prop(Type,Head,_),
	  translate_clauses(Clauses,TClauses),
          DeclAndCode = [ (:- Dir),
	                  (:- type(S,Head))
		        | TClauses ],
	  !
	;
	  display('{ERROR (type_exp): syntax in typedef decl for '),
	  display(Type),
	  display('}'), nl ).
expand_types((:- Dir), (:- Dir)) :-
	\+ found_type_decl(_),
	is_type_decl((:- Dir), T),
	!,              %% (else fail, and this expansion not performed)
	start_parsing_type(T).
expand_types(Clause, Clause) :-
	found_type_decl(H),
	is_clause_or_fact(Clause,H),
	!,
	retract_fact(type_clauses(PrevClauses)),
	asserta_fact(type_clauses([Clause|PrevClauses])).
expand_types(Clause, NewClauses) :-
	found_type_decl(H),
	\+ ( is_clause_or_fact(Clause,H) ),
	!,
	gather_clauses(Clause,NewClauses).

translate_clauses([],[]) :-
	!.
translate_clauses([clause(Head,true)|R],[Head|TR]) :-
	!,
	translate_clauses(R,TR).
translate_clauses([clause(Head,Body)|R],[':-'(Head,Body)|TR]) :-
	!,
	translate_clauses(R,TR).

is_clause_or_fact((H :- _),H).
is_clause_or_fact( H      ,H) :-
	\+ (H = (:- _)).

is_typedef(typedef('::='(Type,Def)),Type,Def,check).
is_typedef(typedef(S,'::='(Type,Def)),Type,Def,S).

%% If a '+' field is present by not recognizing it here as is_type_decl 
%% we simply leave it as is!
is_type_decl( (:- type((T ; _)))   , T).
is_type_decl( (:- type((T # _)))   , T). %% Added MH
is_type_decl( (:- type(_,(T ; _))) , T).
is_type_decl( (:- type(_,(T # _))) , T). %% Added MH
is_type_decl( (:- type(T))         , T).
is_type_decl( (:- type(_,T))       , T).

start_parsing_type(T) :-
	( T = F/A, functor(H,F,A)
	; functor(T,F,A), functor(H,F,A) ),
	!,
	asserta_fact(found_type_decl(H)),
	asserta_fact(type_clauses([])).

gather_clauses(Clause,NewClauses) :-
	retract_fact(found_type_decl(H)),
	type_clauses(Clauses),
	(  denorm_goal_prop(H,TT,_),
           regular_type_pred_definition_e(Clauses, Error),
           var(Error),
	   pred_to_typedef(Clauses,TT,Rule),
	   is_typedef(Typedef,TT,Rule,check)
	-> NewClauses = [ (:- Typedef), Clause ]
 	;  NewClauses = [ Clause ],
	   functor(H,F,A), 
	   display('{ERROR (type_exp): '),
	   display(F/A),
	   display(' is not a regular type}'), nl ),
	(  is_type_decl( Clause, NT )
	-> start_parsing_type(NT)
	;  true ).

norm_goal_prop(GP,NGP,NPD) :-
	GP  =..[F|Args],
	!,
	NGP =.. [F,NPD|Args].

denorm_goal_prop(NGP,GP,NPD) :-
	NGP =.. [F,NPD|Args],
	!,
	GP  =..[F|Args].

append([],X,X).
append([X|Y],Z,[X|W]) :- append(Y,Z,W).

%% %% ----- Inline_code -------------------------------------------
%% 
%% typedef_to_pred((Or; Ors), Pred, [Clause|Cls]):- 
%%         !,
%%         one_disjunct_to_clause(Or, Pred, Clause),
%% 	typedef_to_pred(Ors, Pred, Cls).
%% typedef_to_pred(Or, Pred, [Clause]):-
%%         one_disjunct_to_clause(Or, Pred, Clause).
%% 
%% one_disjunct_to_clause(Or, Pred, clause(G, Body)):-
%%         copy_term((Or,Pred), (CopOr,CopPred)), 
%%         create_param_type_call(CopPred, X, G),
%% 	typedef_to_pred0(CopOr, X, Body).
%% 
%% create_param_type_call(ParType, Arg, TypeCall):-
%%         ParType =.. [Name|TypeArgs],
%%         TypeCall =.. [Name, Arg|TypeArgs].
%% 
%% typedef_to_pred0(Or, X, Body):-
%%         typedef_to_pred1(Or, X, Body1),
%%         (var(Body1) -> Body = true ; Body = Body1).
%% 
%% typedef_to_pred1(Type, CType, _):- 
%%         constant_symbol(Type, CType), 
%%         !.
%% typedef_to_pred1(Term, X, Body):- 
%%         atom(Term),
%%         \+ Term = [],
%%         !,
%%         Body=..[Term, X].
%% typedef_to_pred1(Term, X, Body):- 
%%         par_rule_type_symbol(Term),
%%         !,
%%         create_param_type_call(Term, X, Body).
%% typedef_to_pred1(Type, X, Body):-
%%         compound_pure_type_term(Type, Term, Name, Arity),
%%         !,
%%         functor(X, Name, Arity),
%%         typedef_to_pred_compound(1, Arity, Term, X, Body).
%% typedef_to_pred1(Term, X, Body):- 
%%         var(Term),
%%         !,
%%         Body = type(X, Term). 
%% 
%% typedef_to_pred_compound(N, A, Term, X, Lit):- 
%%         N = A, !,
%%         arg(N, Term, ArgT),
%% 	arg(N, X, ArgX),
%% 	typedef_to_pred1(ArgT, ArgX, Lit).
%% typedef_to_pred_compound(N, A, Term, X, Body):-
%% 	N < A, 
%%         !,
%%         arg(N, Term, ArgT),
%% 	arg(N, X, ArgX),
%% 	typedef_to_pred1(ArgT, ArgX, Lit),
%%         (nonvar(Lit) -> 
%%             Body = (Lit, Body1)
%%             ;
%%             Body = Body1),
%% 	N1 is N + 1,
%% 	typedef_to_pred_compound(N1, A, Term, X, Body1).
%% 
%% constant_symbol(Type, CType):- 
%%    number_constant(Type, CType); atom_constant(Type, CType).
%% 
%% number_constant(Type, Type):- 
%%    number(Type).
%% 
%% atom_constant(Type, Type):-
%%      nonvar(Type), 
%%      Type = [], 
%%      !.
%% atom_constant(Type, Term):- 
%%    nonvar(Type),
%%    Type = ^(Term),
%%    atom(Term),
%%    \+ Term = any,
%%    \+ Term = bot,
%%    \+ Term = ground,
%%    \+ Term = number,
%%    \+ Term = atom,
%%    \+ Term = var,
%%    \+ Term = [].
%% 
%% par_rule_type_symbol(Type):-
%%    nonvar(Type),
%%    functor(Type, F, A),
%%    A > 0,
%%    \+ F/A = (^)/1,
%%    \+ F/A = (.)/2.
%%    
%% compound_pure_type_term(Type, Term, Name, Arity):- 
%%    nonvar(Type),
%%    Type = ^(Term),
%%    nonvar(Term),
%%    functor(Term, Name, Arity),
%%    Arity > 0,
%%    \+ Term = [_|_], 
%%    !.
%% compound_pure_type_term(Type, Type, (.), 2):- 
%%    nonvar(Type),
%%    Type = [_|_].
%% 
%% %% ------------------------------------------------------------------------

:- comment(module,"This module, in conjunction with the
   @library{assertions/types} include file, inserts the syntax
   transformations needed to support @concept{type definitions} in
   source code. This includes types defined as predicates as well as
   types defined using ``@concept{typedef} syntax''.

   These files provide suitable operator definitions for @decl{type}
   and @dec{typedef} declarations, expand @tt{typedef} declarations
   into a @decl{type} declaration plus the corresponding code (so that
   the defined type can be used as a run-time check), and perform very
   limited checking of the type definitions at read time. Full
   checking of types should be done with the preprocessor.  The form
   in which this code is written imposes certain restrictions on how
   type definitions should appear in the program. In particular, the
   @tt{type} declaration must appear in the program before the clauses
   that define the type. The clauses that define the type must be
   contiguous: it is assumed that the type definition finishes as soon
   as any other declaration or clauses for another predicate are
   found.

").
