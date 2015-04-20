:- module(_,[sql_persistent_tr/2, sql_goal_tr/2,
	dbId/2],[assertions]).

:- use_module(library(dynamic)).
:- use_module(library(lists),[append/3]).
:- use_module(library('persdb_mysql_op/persdbrt_mysql_op'),
	[assert_args/4]).
:- use_module(library('persdb_mysql_op/pl2sql')).
:- use_module(library('persdb_mysql_op/sql_types'),
	[sql_type/1, sql_compatible/2]).
:- use_module(library(prolog_sys),[new_atom/1]).
:- use_module(library(write),[writeq/1]).
:- use_module(library(vndict),[create_dict/2]).

:- data dbId/2.

:- multifile([relation/3,attribute/4]).
:- data([relation/3,attribute/4]).

:- set_prolog_flag(multi_arity_warnings,off).
:- set_prolog_flag(write_strings,on).

is_true(true(Info),Info).
is_true(true).
is_ground('props:ground'(G),G).
is_ground(ground(G),G).

%% In source code both sql_persistent/3 directives and '$is_sql_persistent'/3 
%% facts may appear. sql_persistent/3 directive will be present if the current
%% module has not been preprocessed before, while '$is_sql_persistent' facts 
%% will appear after analyzing the code (because
%% sql_persistent/3 directives have been expanded to $is_sql_persistent facts.)
%% This package must include code for both cases.
sql_persistent_tr( ('$is_sql_persistent'(PrologDef,SQLDef,DBId):- Body),
	           ('$is_sql_persistent'(PrologDef,SQLDef,DBId):- Body)):-
        sql_persistent_tr_2(PrologDef,SQLDef,Skel),
	assertz_fact(dbId(DBId,Skel)).
sql_persistent_tr( (:- sql_persistent(PrologDef,SQLDef,DBId)), ClauseList) :-
	\+ dbId(_,PrologDef), % It has not been already expanded (due to analysis preprocessing). 
	sql_persistent_tr_2(PrologDef,SQLDef,Consequence),
	ClauseList =  [ '$is_sql_persistent'(PrologDef,SQLDef,DBId),
%jcf-deterministic-db-calls-begin
% 10/06/03: change made to check if a call is deterministic.
%           We use prefetch/3 to get a db tuple in advance. 
% 			(Consequence :- db_call_db_atomic_goal(DBId,Consequence)) ].
 			(Consequence :- prefetch_db_call(DBId,Consequence)) ],
%	display(ClauseList),nl.
	true.
%jcf-deterministic-db-calls-end
sql_persistent_tr((:- true pred Assertion), ClauseList) :-
	get_persistence_info(Assertion,PrologDef,SQLDef,DBId),
%	writeq((:- true pred Assertion)),nl,
	\+ dbId(_,PrologDef), % It has not been already expanded (due to analysis preprocessing). 
	sql_persistent_tr_2(PrologDef,SQLDef,Consequence),
	ClauseList =  [ 
			  (:- true pred Assertion),
			  '$is_sql_persistent'(PrologDef,SQLDef,DBId),
% 10/06/03: change made to check if a call is deterministic.
%           We use prefetch/3 to get a db tuple in advance. 
% 			  (Consequence :- db_call_db_atomic_goal(DBId,Consequence)) ].
 			  (Consequence :- prefetch_db_call(DBId,Consequence))
%jcf-deterministic-db-calls-end
		      ],
%	writeq(ClauseList),nl.
	true.
sql_persistent_tr( (Head :- Body), (Head :- NewBody)) :-
	group_persistent_goals(Body,Body1),
	remove_trues(Body1,Body2),
	transform_body(Body2,NewBody),
%	writeq((Head :- NewBody)),nl.
	true.
sql_persistent_tr(end_of_file,end_of_file) :-
	retractall_fact(relation(_,_,_)),
	retractall_fact(attribute(_,_,_,_)),
	retractall_fact(dbId(_,_)).

%%---------------

sql_persistent_tr_2(PrologDef,SQLDef,Consequence):-
 	functor(PrologDef,PrologName,Arity),
 	functor(Consequence,PrologName,Arity),
	PrologDef =.. [PrologName | Types],
	SQLDef    =.. [TableName  | ArgNames],
	assertz_fact(relation(PrologName,Arity,TableName)),
	assert_args(Types,ArgNames,1,TableName).

%%---------------
%% Currently assertions are not normalized. Only specific
%% assertions are understood.
get_persistence_info(Ass,PrologDef,SQLDef,DBId):-
	Ass = (Pred :: Types + persistent(DBId,SQLDef)),
	name_arity(Pred,Name,Arity),
	functor(SQLDef,_,Arity),
	functor(PrologDef,Name,Arity),
	get_types(Types,TypesList),
	PrologDef =.. [Name | TypesList].
	
name_arity(Name/Arity,Name,Arity).
name_arity(Pred,Name,Arity):-
	functor(Pred,Name,Arity).

get_types(Type,[SqlType]):-
	is_sql_type(Type,SqlType).
get_types(*(Type,Types),[SqlType|LTypes]):-
	is_sql_type(Type,SqlType),
	get_types(Types,LTypes).

is_sql_type(Type,Type):-
	sql_type(Type).
is_sql_type(Type,SqlType):-
	sql_compatible(Type,SqlType).

%% ----------------------------------------------------------------------------

:- pred group_persistent_goals(+Body,-NewBody) 

# "Given a complex goal @var{Body}, possibly including program point
  analysis info, transforms it grouping persistent atomic goals and
  removing all analysis info at program points except those preceding
  every group of atomic persistent goals. Grouped goals are
  encapsulated in the second argument of @tt{true/2} goals. These
  goals are transformed later bytransform_body/2.".

group_persistent_goals( (True,Goal) , (true(PPInfo,Pers),NewRest) ):-
	is_true(True,PPInfo),
	split_persistent_goals(Goal,Pers,_Db,Rest),
	Pers \== true, !,
	group_persistent_goals(Rest,NewRest).
group_persistent_goals(Goal, (true([],Pers),NewRest) ):-
	split_persistent_goals(Goal,Pers,_Db,Rest),
	Pers \== true, !,
	group_persistent_goals(Rest,NewRest).
group_persistent_goals( (A,B) , (NewA,NewB)):- !,
	group_persistent_goals(A,NewA),
	group_persistent_goals(B,NewB).
group_persistent_goals( (A;B) , (NewA;NewB)):- !,
	group_persistent_goals(A,NewA),
	group_persistent_goals(B,NewB).
group_persistent_goals( (A->B) , (NewA->NewB)):- !,
	group_persistent_goals(A,NewA),
	group_persistent_goals(B,NewB).
group_persistent_goals(A,A).

%% ----------------------------------------------------------------------------

:- pred split_persistent_goals(Goal,Pers,Db,Rest) 

# "Given a complex goal @var{Goal}, splits it in two goals: @var{Pers}
  is the subgoal that corresponds to the initial sequence of atomic
  goals which are persistent (and belong to a single database
  @var{Db}); and @var{Rest} is the sequence of remaining goals (those
  next to the last persistent goal). No goal reordering is made; just
  goal grouping.".

split_persistent_goals((True,Goal),Pers,Db,Rest):-
	is_true(True,_), !,
	split_persistent_goals(Goal,Pers,Db,Rest).
split_persistent_goals(Goal,Goal,Db,true):-
	complex_goal_persistent(Goal,Db), !.
split_persistent_goals( (A,B) , (A,PersB) ,Db,Rest):-
	complex_goal_persistent(A,Db),!,
	split_persistent_goals(B,PersB,Db,Rest).
split_persistent_goals( (A,B) ,Pers,Db,Rest):-
	split_persistent_goals(A,Pers,Db,RestA), !,
	Rest = (RestA,B).
split_persistent_goals(True,true,_,true):-
	is_true(True,_), !.
split_persistent_goals(Goal,true,_,Goal).

%% ----------------------------------------------------------------------------

:- pred complex_goal_persistent(Goal,Db) 

# "Succeeds if @var{Goal} is formed exclusively by persistent atomic
  goals of a single database @var{Db}.".


complex_goal_persistent((A,B),Db):-
	complex_goal_persistent(A,Db),
	complex_goal_persistent(B,Db).
complex_goal_persistent((A;B),Db):-
	complex_goal_persistent(A,Db),
	complex_goal_persistent(B,Db).
complex_goal_persistent((A->B),Db):-
	complex_goal_persistent(A,Db),
	complex_goal_persistent(B,Db).
complex_goal_persistent(A,Db):-
 	dbId(Db,A).

%% ----------------------------------------------------------------------------

:- pred remove_trues(Goal,NewGoal) 

# "Removes unneeded true info (true/0, true/1 and true(_,true))".

remove_trues((True,Goal),NewGoal):-
	(is_true(True) ; is_true(True,_) ; True = true(_,true)),!,
	remove_trues(Goal,NewGoal).
remove_trues((Goal,True),NewGoal):-
	(is_true(True) ; is_true(True,_) ; True = true(_,true)),!,
	remove_trues(Goal,NewGoal).
remove_trues((A,B),(NewA,NewB)):- !,
	remove_trues(A,NewA),
	remove_trues(B,NewB).
remove_trues((A;B),(NewA;NewB)):- !,
	remove_trues(A,NewA),
	remove_trues(B,NewB).
remove_trues((A->B),(NewA->NewB)):- !,
	remove_trues(A,NewA),
	remove_trues(B,NewB).
remove_trues(true(_,Goal),true):- 
	remove_trues(Goal,true),!.
remove_trues(true(PPInfo,Goal),true(PPInfo,NewGoal)):- !,
	remove_trues(Goal,NewGoal).
remove_trues(True,true):- 
	(is_true(True,_) ; True = true(_,true)),!.
remove_trues(G,G).

%% ----------------------------------------------------------------------------

:- pred transform_body(Goal,NewGoal) 

# "Complex goal @var{Goal} is transformed to @var{NewGoal}, removing
  @tt{true/1} goals and using their arguments to optimize calls to
  persistent predicates. It is assumed that a complex conjunctive goal
  is of the form (Goal1,(Goal2,(...,Goaln)...))".

transform_body(true(PPInfo,Goal),NewGoal):-
	extract_ground_info(PPInfo,GroundVars),
 	translate_goal(Goal,GroundVars,NewGoal).
transform_body((A,B),(NewA,NewB)):- !,
	transform_body(A,NewA),
	transform_body(B,NewB).
transform_body((A;B),(NewA;NewB)):- !,
	transform_body(A,NewA),
	transform_body(B,NewB).
transform_body((A->B),(NewA->NewB)):- !,
	transform_body(A,NewA),
	transform_body(B,NewB).
transform_body(!,!):- !.
transform_body(Goal,Goal).

%% ----------------------------------------------------------------------------

:- pred extract_ground_info(+PPInfo,-GroundVars) 

# "Given a list of program point info @var{PPInfo}, return the list of
  ground variables @var{GroundVars}.".

extract_ground_info((A,B),G):- !,
	extract_ground_info(A,GA),
	extract_ground_info(B,GB),
	append(GA,GB,G).
extract_ground_info(Ground,G):-
	is_ground(Ground,G), !.
extract_ground_info(_,[]).

%% ----------------------------------------------------------------------------

:- pred translate_goal(+Goal,+GroundVars,-NewGoal)

# "Given a persistent goal @var{Goal} and a list of ground vars
  @var{GroundVars}, to call directly to the sql query.".

translate_goal(Goal,GroundVars,NewGoal):-
	copy_term(pair(Goal,GroundVars),pair(GGoal,GVars)),
	build_proj_term(GGoal,PTerm),    % A proj. term is built with all vars. (free or ground)
	groundify(GVars),                % Ground variables are asigned a key
	copy_term(pair(GGoal,PTerm),pair(GroundGoal,ProjTerm)),
					 % pl2sql instantiates free vars, so we need another copy.
	create_dict(ProjTerm,dic(ProjVars,_)), % ProjVars is the list of free variables.
	pl2sqlstring(PTerm,GGoal,SQLStrQuery),
 	dbId(DBId,_), %%% MAL!!! (supone que solo hay una bd!!)
%jcf-deterministic-db-calls-begin
%%%	  new_atom(CallKey),
%jcf-deterministic-db-calls-end
	( GVars = [] ->
	  GroundGoal = Goal,
	  NewGoal = (
		      ( varlist(ProjVars) ->
%jcf-deterministic-db-calls-begin
%			sql_query_one_tuple(DBId,SQLStrQuery,__ResultTuple),
			prefetch_sql_query_one_tuple(DBId,SQLStrQuery,__ResultTuple),
%jcf-deterministic-db-calls-end
			ProjTerm =.. [_|__ResultTuple]
		      ; Goal
		      )
		    )
        ; NewGoal = (
		      ( varlist(ProjVars) ->
			replace_marks(GroundGoal,Goal,SQLStrQuery,__SQLString),
%jcf-deterministic-db-calls-begin
%			sql_query_one_tuple(DBId,__SQLString,__ResultTuple),
			prefetch_sql_query_one_tuple(DBId,__SQLString,__ResultTuple),
%jcf-deterministic-db-calls-end
			ProjTerm =.. [_|__ResultTuple]
		      ; Goal
		      )
		    )
	).

%% ----------------------------------------------------------------------------
	
:- pred groundify(VarList) 

# "Instantiates the variables in @var{VarList} to their names.".

groundify([]).  
groundify([V|Vs]):-
	new_atom(NewAtom),
	atom_concat('persdb_opt$',NewAtom,V),
	groundify(Vs).

%% ----------------------------------------------------------------------------

:- pred build_proj_term(+Goal,-ProjTerm) 

# "Given a (possibly complex) goal @var{Goal}, builds a projection
  term @var{ProjTerm}, a term that gathers the variables which appear
  in the goal.".

build_proj_term(Goal,ProjTerm):-
	create_dict(Goal,dic(Dict,_)),
	ProjTerm =.. [proj_term|Dict].

%jcf.09.05.2003-end
%jcf	
% If a persistent predicate is also defined as a normal predicate inside the same module, 
% queries to that predicate should provide wrong results (first database rows, then 
% prolog clauses), but it doesn't. THIS MUST BE CHECKED!

sql_goal_tr( Goal, db_call_db_atomic_goal(DBId,Goal)) :-
	current_fact(dbId(DBId,Goal)).


:- set_prolog_flag(multi_arity_warnings,on).
