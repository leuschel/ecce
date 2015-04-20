:- module(pl2sqlBAK,
	[
	    pl2sqlstring/3,
	    querybody/1,
	    projterm/1,
	    sqlstring/1,
	    pl2sqlterm/3,
	    printqueries/1,
	    sqlterm2string/2,
	    sqltype/1
	],[assertions, types, basicmodes]).
%% :- include(library(assertions)).
%% :- include(library(types)).
%% :- include(library(basicmodes)).

:- multifile [relation/3,attribute/4].
:- data [relation/3,attribute/4].


%% TEMPORARY
:- use_module(library(write)).
:- use_module(library(read)).
:- use_module(library(dec10_io)).

:- use_module(library(lists),[list_concat/2,append/3]).

:- use_module(library(iso_misc)).
%%:- use_module(library(basicprops)).
:- use_module(library(aggregates)).
:- use_module(library(messages),[debug_message/2,error_message/2]).

:- comment(bug,"Need to separate db predicate names by module.").

:- comment(bug,"Need to fix types to be compatible with ciao, i.e.,
   strings are really atoms and stuff like that...").

:- comment(title,"Prolog to SQL Translator").

:- comment(author,"C. Draxler. Adapted by M. Hermenegildo and I. Caballero").

:- comment(module,"This library performs a translation of Prolog
   queries into @concept{SQL} code. The code is an adaptation for CIAO
   of the @concept{Prolog to SQL compiler} written by Christoph
   Draxler, CIS Centre for Information and Speech Processing,
   Ludwig-Maximilians-University Munich,
   @tt{draxler@@cis.uni-muenchen.de}, Version 1.1. Many thanks to
   Christoph for allowing us to include this adaptation of his code
   with CIAO.

   The translator needs to know the correspondence between Prolog
   predicates and the @concept{SQL tables} in the database. To this
   end this module exports two multifile predicates, @pred{relation/3}
   and @pred{attribute/4}. See the description of these predicates for
   details on how such correspondance is specified.

   The main entry points to the translator are @pred{pl2sqlstring/3}
   and @pred{pl2sqlterm/3}. Details on the types of queries allowed
   can be found in the description of these predicates.

   @bf{Example:} the following program would print out a term
   representing the SQL query  corresponding to the 
   given Prolog query:

@begin{verbatim}
@includeverbatim{/home/clip/Systems/ciao/library/persdb_sql/pl2sql_example.pl}
@end{verbatim}

   @bf{Note:} while the translator can be used directly in programs,
   it is more convenient to use a higher-level abstraction:
   @concept{persistent predicates} (implemented in the @lib{persdb}
   and @lib{persdb_sql} libraries). The notion of persistent
   predicates provides a completely transparent interface between
   Prolog and relational databases. When using this library, the
   Prolog to SQL translation is called automatically as needed.

").

% ----------------------------------------------------------------------------

:- pred relation(PredName, Arity, TableName) :: atm * int * atm

   # "This predicate, together with @pred{attribute/4}, defines the
      correspondence between Prolog predicates and the @concept{SQL
      tables} in the database. These two relations constitute an
      extensible meta-database which maps @concept{Prolog predicate
      names} to @concept{SQL table names}, and @concept{Prolog
      predicate argument positions} to @concept{SQL attributes}.

      @var{PredName} is the chosen Prolog name for an SQL
      table. @var{Arity} is the number of arguments of the predicate.
      @var{TableName} is the name of the SQL table in the Database
      Management System.".

% ---------------------------------------------------------------------------

:- pred attribute(ANumber, TblName, AName, AType) :: int * atm * atm * sqltype 

   # "This predicate maps the argument positions of a Prolog predicate
      to the SQL attributes of its corresponding table. The types of
      the arguments need to be specified, and this information is used
      for consistency checking during the translation and for output
      formatting.  A minimal type system is provided to this end.  The
      allowable types are given by @pred{sqltype/1}.

      @var{ANumber} is the argument number in the Prolog
      relation. @var{TblName} is the name of the SQL table in the
      Database Management System. @var{AName} is the name of the
      corresponding attribute in the table. @var{AType} is the
      (translator) data type of the attribute.".

% ---------------------------------------------------------------------------

:- pred pl2sqlstring(+ProjectionTerm, +DatabaseGoal, -SQLQueryString) 
   :: projterm * querybody * sqlstring

   # "This is the top level predicate which translates complex Prolog
      goals into the corresponding SQL code. 

      The query code is prepared in such a way that the result is
      projected onto the term @var{ProjectionTerm} (also in a similar
      way to the first argument of @pred{setof/3})). See the predicate
      @pred{translate_projection/3} for restrictions on this term.

      @var{SQLQueryString} contains the code of the @concept{SQL
      query}, ready to be sent to an @concept{SQL server}.".

pl2sqlstring(ProjectionTerm,DatabaseGoal,SQLQueryString):-
   pl2sqlterm(ProjectionTerm,DatabaseGoal,SQLQueryTerm),
   debug_message("converting term ~w to string", [SQLQueryTerm]),
   sqlterm2string(SQLQueryTerm,SQLQueryString),
   !.

:- prop querybody(DBGoal) # "@var{DBGoal} is a database query goal.".

querybody( DBGoal ) :- 
	callable(DBGoal).

:- comment(querybody/1,"@var{DBGoal} is a goal meant to be executed in
   the external database. It can be a complex term containing
   @concept{conjunctions}, @concept{disjunctions}, and
   @concept{negations}, of:

   @begin{itemize}

   @item @concept{Atomic goals}, which must have been defined via
   @pred{relation/3} and @pred{attribute/4} and reside in the (same)
   database. Their arguments must be either ground or free
   variables. If they are ground, they must be bound to constants of
   the type declared for that argument. If an argument is a free
   variable, it may @em{share} with (i.e., be the same variable as)
   other free variables in other goal arguments.

   @item @concept{Database comparison goals}, whose main functor must
   be a @concept{database comparison operator} (see
   @lib{pl2sql}:@pred{comparison/2}) and whose arguments must be
   @em{database arithmetic expressions}.

   @item @concept{Database calls to is/2}. The left side of such a
   call may be either unbound, in which case it is bound to the result
   of evaluating the right side, or bound in which case an equality
   condition is tested. The right side must be a @em{database
   arithmetic expression}.

   @end{itemize}

   The binding of variables follows Prolog rules: 

   @begin{itemize} 

   @item variables are bound by positive base goals and on the left
   side of the @pred{is/2} predicate.

   @item Comparison operations, negated goals, and right sides of the
   @pred{is/2} predicate do not return variable bindings and may even
   require all arguments to be bound for a safe evaluation.

   @end{itemize}

   @concept{Database arithmetic expressions} may contain:

   @begin{itemize}

   @item Numeric constants (i.e., integers, reals, etc.).

   @item Bound variables, i.e., variables which will be bound during
   execution through occurrence within a positive database goal, or 
   by a preceding arithmetic function.

   @item @concept{Database arithmetic functions}, which are a subset
   of those typically accepted within @pred{is/2} (see
   @lib{pl2sql}:@pred{arithmetic_functor/2}).

   @item @concept{Database aggregation functions}, each of which has
   two arguments: a variable indicating the argument over which the
   function is to be computed, and a goal argument which must contain
   in at least one argument position the variable (e.g.
   @tt{avg(Seats,plane(Type,Seats))}). The goal argument may only be a
   conjunction of (positive or negative) base goals. See
   @lib{pl2sql}:@pred{aggregate_functor/2} for the admissible
   aggregate functions.

   @end{itemize}

   In addition, variables @cindex{existential quantification} can be
   existentially quantified using @pred{^/2} (in a similar way to how
   it is done in @pred{setof/3}). 

   Note that it is assumed that the arithmetic operators in Prolog and
   SQL are the same, i.e., @tt{+} is addition in Prolog and in SQL,
   etc. 

").

:- prop projterm(DBProjTerm) # "@var{DBProjTerm} is a database
   projection term.".

projterm( DBProjTerm ) :- 
	term(DBProjTerm).

:- comment(projterm/1,"@var{DBProjTerm} is a term onto which the
   result of a database query code is (in a similar way to the first
   argument of @pred{setof/3})). 

   A @var{ProjectionTerm} must meet the following restrictions:

   @begin{itemize}

   @item The functor of @var{ProjectionTerm} may not be one of the
         built-in predicates, i.e. ',', ';', etc. are not allowed.

   @item Only variables and constants are allowed as arguments, i.e.,
         no structured terms may appear.

   @end{itemize}").

:- prop sqlstring(S) # "@var{S} is a string containing SQL code.".

sqlstring( S ) :- 
	string(S).

:- comment(sqlstring/1,"@includedef{sqlstring/1}").

% ---------------------------------------------------------------------------

:- pred pl2sqlterm(+ProjectionTerm, +DatabaseGoal, -SQLQueryTerm) 
   :: projterm * querybody * list(sqlterm)

   # "Similar to @pred{pl2sqlstring/3} except that @var{SQLQueryTerm} is
      a representation of the SQL query as a Prolog term.".

pl2sqlterm(ProjectionTerm,DatabaseGoal,SQLQueryTerm):-

   debug_message("compiling ~w :- ~w",[ProjectionTerm,DatabaseGoal]),

   debug_message("initialize var identifiers and range vars for relations", []),
   init_gensym(variable),
   init_gensym(rel),

   debug_message("tokenizing projection term and database goal", []),
   tokenize_term(DatabaseGoal,TokenDatabaseGoal),
   tokenize_term(ProjectionTerm,TokenProjectionTerm),

   debug_message("lexical analysis - reorder goals for disjunctive normal form", []),
   convert_to_disj_norm_form(TokenDatabaseGoal,Disjunction),
   debug_message("disjunction is ~w",[Disjunction]), %%%% Ignacio
   debug_message("code generation", []),
   query_generation(Disjunction,TokenProjectionTerm,SQLQueryTerm),
   debug_message("SQL Query term is ~w",[SQLQueryTerm]),%%%% Ignacio
   debug_message("pl2sqlterm/3 done", []),
   !.
pl2sqlterm(ProjectionTerm,DatabaseGoal,_SQLQueryTerm) :-
   error_message("SQL translation failed for ~q / ~q",[ProjectionTerm,DatabaseGoal]),
   fail.


% ---------------------------------------------------------------------------

:- pred convert_to_disj_norm_form(+Goal,-Disjunction) 

# "Turns the original goal into disjunctive normal form by computing a
    set of flat conjunctions and collecting them in a list, whose
    elements are assumed to be joined by disjuntions.".

convert_to_disj_norm_form(Goal,Disjunction):-
   findall(Conjunction,linearize(Goal,Conjunction),Disjunction).

% ---------------------------------------------------------------------------

:- pred linearize(+Goal,-ConjunctionList)

# "Returns a conjunction of base goals for a complex disjunctive or
   conjunctive goal. Yields several solutions upon backtracking for
   disjunctive goals.".

linearize(((A,B),C),(LinA,(LinB,LinC))):-
   % --- transform left-linear to right-linear conjunction (',' is associative)
   linearize(A,LinA),
   linearize(B,LinB),
   linearize(C,LinC).
linearize((A,B),(LinA,LinB)):-
   A \= (_,_),
   % --- make sure A is not a conjunction ------------------------------------
   linearize(A,LinA),
   linearize(B,LinB).
linearize((A;_B),LinA):-
   linearize(A,LinA).
linearize((_A;B),LinB):-
   linearize(B,LinB).
linearize(\+(A),\+ (LinA)):-
   linearize(A,LinA).
linearize(Var^A, Var^LinA):-
   linearize(A,LinA).
linearize(A,A):-
   A \= (_,_),
   A \= (_;_),
   A \= _^_,
   A \= \+(_).

% ---------------------------------------------------------------------------

:- pred tokenize_term(+Term,-TokenizedTerm)

# "If Term is a

   @begin{itemize}

   @item @em{variable}, then this variable is instantiated with a
   unique identifier of the form '$var$'(VarId), and
   @var{TokenizedTerm} is bound to the same term '$var$'(VarId).

   @item @em{constant}, then @var{TokenizedTerm} is bound to
   '$const$'(Term).

   @item @em{complex term}, then the term is decomposed, its arguments
   are tokenized, and @var{TokenizedTerm} is bound to the result of
   the composition of the original functor and the tokenized
   arguments.

   @end{itemize}".

tokenize_term('$var$'(VarId),'$var$'(VarId)):-
   var(VarId),
   % --- uninstantiated variable: instantiate it with unique identifier.
   gensym(variable,VarId).
tokenize_term('$var$'(VarId),'$var$'(VarId)):-
   nonvar(VarId),
   %% with cut,  '$const$'(var22) is not instantiated. ^^
   !. 
tokenize_term(Constant,'$const$'(Constant)):-
   nonvar(Constant),
   functor(Constant,_,0).
tokenize_term(Term,TokenizedTerm):-
   nonvar(Term),
   Term \= '$var$'(_),
   Term \= '$const$'(_),
   Term =.. [Functor|Arguments],
   Arguments \== [],
   tokenize_arguments(Arguments,TokenArguments),
   TokenizedTerm =.. [Functor|TokenArguments].

% ---------------------------------------------------------------------------

:- pred tokenize_arguments(Arguments,TokenizedArguments)

# "Organizes tokenization of arguments by traversing the argument list
   and calling tokenize_term for each element of the list.".

tokenize_arguments([],[]).
tokenize_arguments([FirstArg|RestArgs],[TokFirstArg|TokRestArgs]):-
   tokenize_term(FirstArg,TokFirstArg),
   tokenize_arguments(RestArgs,TokRestArgs).

% ---------------------------------------------------------------------------

:- comment(doinclude,query_generation/3).

:- pred query_generation(+ListOfConjunctions, +ProjectionTerm, -ListOfQueries)

# "For each Conjunction in @var{ListOfConjunctions}, translate the pair
   @tt{(ProjectionTerm,Conjunction)} to an SQL query and connect each
   such query through a @concept{UNION-operator} to result in the
   @var{ListOfQueries}.

   A Conjunction consists of positive or negative subgoals. Each
   subgoal is translated as follows:

   @begin{itemize} 

   @item the @concept{functor of a goal} that is not a comparison
         operation is translated to a @concept{relation name} with a
         @concept{range variable},

   @item @concept{negated goals} are translated to @concept{NOT
         EXISTS-subqueries} with @concept{* projection},

   @item @concept{comparison operations} are translated to comparison
         operations in the @concept{WHERE-clause},

   @item @concept{aggregate function terms} are translated to
         @concept{aggregate function (sub)queries}.

   @end{itemize}

   The arguments of a goal are translated as follows:

   @begin{itemize} 

   @item @var{variables of a goal} are translated to @concept{qualified
         attributes},

   @item @concept{variables occurring in several goals} are translated
         to equality comparisons (equi join) in the WHERE-clause,
         @cindex{equi join in the WHERE-clause}

   @item @concept{constant arguments} are translated to
         @concept{equality comparisons in the WHERE-clause}.

   @end{itemize}

   Arithmetic functions are treated specially
   (@pred{translate_arithmetic_function/5}). See also
   @prop{querybody/1} for details on the syntax accepted and
   restrictions.

".

query_generation([],_,[]).
query_generation([Conjunction|Conjunctions],ProjectionTerm,[Query|Queries]):-
   check_projection_term_variables(ProjectionTerm,InitDict),
   check_translate_conjunction(Conjunction,SQLFrom,SQLWhere,InitDict,Dict),
   check_translate_projection(ProjectionTerm,Dict,SQLSelect),
   Query = query(SQLSelect,SQLFrom,SQLWhere),
   check_query_generation(Conjunctions,ProjectionTerm,Queries).

check_projection_term_variables(ProjectionTerm,InitDict) :-
   projection_term_variables(ProjectionTerm,InitDict),
   !.
check_projection_term_variables(ProjectionTerm,_InitDict) :-
   error_message("in projection term ~w",[ProjectionTerm]),
   fail.

check_translate_conjunction(Conjunction,SQLFrom,SQLWhere,InitDict,Dict) :-
   translate_conjunction(Conjunction,SQLFrom,SQLWhere,InitDict,Dict),
   !.
check_translate_conjunction(Conjunction,_SQLFrom,_SQLWhere,_InitDict,_Dict) :-
   error_message("in conjunction ~w",[Conjunction]),
   fail.

check_translate_projection(ProjectionTerm,Dict,SQLSelect) :-
   translate_projection(ProjectionTerm,Dict,SQLSelect),
   !.
check_translate_projection(ProjectionTerm,_Dict,_SQLSelect) :-
   error_message("in projection term ~w",[ProjectionTerm]),
   fail.

check_query_generation(Conjunctions,ProjectionTerm,Queries) :-
   query_generation(Conjunctions,ProjectionTerm,Queries),
   !.
check_query_generation(Conjunctions,ProjectionTerm,_Queries) :-
   error_message("in ~w or ~w",[ProjectionTerm,Conjunctions]),
   fail.

% ---------------------------------------------------------------------------

:- pred projection_term_variables(ProjectionTerm,Dict)

# "Extracts all variables from the @var{ProjectionTerm} and places
   them into the @var{Dict} as a dict/4 term with their Identifier, a
   non instantiated RangeVar and Attribute argument, and the keyword
   existential for the type of quantification.".

projection_term_variables('$const(_)$',[]).
projection_term_variables('$var$'(VarId),[dict(VarId,_,_,_,existential)]).
projection_term_variables(ProjectionTerm,ProjectionTermVariables):-
   ProjectionTerm =.. [Functor|ProjectionTermList],
   \+ (Functor = '$var$'),
   \+ (ProjectionTermList = []),
   projection_list_vars(ProjectionTermList,ProjectionTermVariables).

projection_list_vars([],[]).
projection_list_vars(['$var$'(VarId)|RestArgs],
                     [dict(VarId,_,_,_,existential)|RestVars]):-
   projection_list_vars(RestArgs,RestVars).
projection_list_vars(['$const$'(_)|RestArgs],Vars):-
   projection_list_vars(RestArgs,Vars).

% ---------------------------------------------------------------------------

:- pred translate_projection(ProjectionTerm,Dict,SelectList)

# "Translates the projection term.".

translate_projection('$var$'(VarId),Dict,SelectList):-
   projection_arguments(['$var$'(VarId)],SelectList,Dict).
translate_projection('$const$'(Const),_,['$const$'(Const)]).
translate_projection(ProjectionTerm,Dict,SelectList):-
   ProjectionTerm =.. [Functor|Arguments],
   \+ (Functor = '$var$'),
   \+ (Functor = '$const$'),
   \+ (Arguments = []),
   projection_arguments(Arguments,SelectList,Dict).

projection_arguments([],[],_).
projection_arguments([Arg|RestArgs],[Att|RestAtts],Dict):-
   retrieve_argument(Arg,Att,Dict),
   projection_arguments(RestArgs,RestAtts,Dict).


% ---------------------------------------------------------------------------

:- pred retrieve_argument(Argument,SQLAttribute,Dictionary) 

# "Retrieves the mapping of an argument to the appropriate SQL
  construct, i.e.,

  @begin{itemize}
  @item qualified attribute names for variables in base goals,

  @item arithmetic expressions for variables in arithmetic goals,

  @item constant values for constants.
  @end{itemize}
". 

retrieve_argument('$var$'(VarId),Attribute,Dict):-
   lookup(VarId,Dict,TableName,AttName,_),
   (
    TableName = is ->
      Attribute = AttName
   ;
      Attribute = att(TableName,AttName)
   ).
retrieve_argument('$const$'(Constant),'$const$'(Constant),_).

% ---------------------------------------------------------------------------

:- comment(doinclude,translate_conjunction/5).

:- pred translate_conjunction(Conjunction,SQLFrom,SQLWhere,Dict,NewDict)

# "Translates a conjunction of goals (represented as a list of goals
   preceeded by existentially quantified variables) to
   @concept{FROM-clauses} and @concept{WHERE-clauses} of an SQL query.
   A dictionary containing the associated SQL table and attribute
   names is built up as an accumulator pair (arguments @var{Dict} and
   @var{NewDict}).".


translate_conjunction('$var$'(VarId)^Goal,SQLFrom,SQLWhere,Dict,NewDict):-
   % --- add info on existentially quantified variables to dictionary here --
   add_to_dictionary(VarId,_,_,_,existential,Dict,TmpDict),
   translate_conjunction(Goal,SQLFrom,SQLWhere,TmpDict,NewDict).
translate_conjunction(Goal,SQLFrom,SQLWhere,Dict,NewDict):-
   Goal \= (_,_),
   check_translate_goal(Goal,SQLFrom,SQLWhere,Dict,NewDict).
translate_conjunction((Goal,Conjunction),SQLFrom,SQLWhere,Dict,NewDict):-
   check_translate_goal(Goal,FromBegin,WhereBegin,Dict,TmpDict),
   translate_conjunction(Conjunction,FromRest,WhereRest,TmpDict,NewDict),
   append(FromBegin,FromRest,SQLFrom),
   append(WhereBegin,WhereRest,SQLWhere).

check_translate_goal(Goal,SQLFrom,SQLWhere,Dict,NewDict) :-
   translate_goal(Goal,SQLFrom,SQLWhere,Dict,NewDict),
   !.
check_translate_goal(Goal,_SQLFrom,_SQLWhere,_Dict,_NewDict) :-
   error_message("in database goal ~w",[Goal]),
   fail.

% ---------------------------------------------------------------------------

:- comment(doinclude,translate_goal/5).

:- pred translate_goal(Goal,SQLFrom,SQLWhere,Dict,NewDict) 

# "Translates:

   @begin{itemize} 

   @item a @concept{positive database goal} to the associated FROM-
         and WHERE clause of an SQL query,

   @item a @concept{negated database goal} to a negated existential
         subquery,

   @item an @concept{arithmetic goal} to an arithmetic expression or
         an aggregate function query,

   @item a @concept{comparison goal} to a comparison expression, and 

   @item a @concept{negated comparison goal} to a comparison
         expression with the opposite comparison operator.

   @end{itemize} ".

translate_goal(SimpleGoal,[SQLFrom],SQLWhere,Dict,NewDict):-
   % --- positive goal binds variables - bindings are held in the dictionary -
   functor(SimpleGoal,Functor,Arity),
   translate_functor(Functor,Arity,SQLFrom),
   SimpleGoal =.. [Functor|Arguments],
   translate_arguments(Arguments,SQLFrom,1,SQLWhere,Dict,NewDict).

translate_goal(Result is Expression,[],SQLWhere,Dict,NewDict):-
   translate_arithmetic_function(Result,Expression,SQLWhere,Dict,NewDict).

translate_goal(\+(NegatedGoals),[],SQLNegatedSubquery,Dict,Dict):-
   % --- negated goals do not bind variables - hence Dict is returned unchanged
   functor(NegatedGoals,Functor,_),
   \+(comparison(Functor,_)),
   translate_conjunction(NegatedGoals,SQLFrom,SQLWhere,Dict,_),
   SQLNegatedSubquery = [negated_existential_subquery([*],SQLFrom,SQLWhere)].

translate_goal(\+(ComparisonGoal),[],SQLCompOp,Dict,Dict):-
   % --- comparison operations do not bind vars - Dict is returned unchanged
   ComparisonGoal =.. [ComparisonOperator,LeftArg,RightArg],
   comparison(ComparisonOperator,SQLOperator),
   negated_comparison(SQLOperator,SQLNegOperator),
   translate_comparison(LeftArg,RightArg,SQLNegOperator,Dict,SQLCompOp).

translate_goal(ComparisonGoal,[],SQLCompOp,Dict,Dict):-
   % --- comparison operations do not bind vars - Dict is returned unchanged 
   ComparisonGoal =.. [ComparisonOperator,LeftArg,RightArg],
   comparison(ComparisonOperator,SQLOperator),
   translate_comparison(LeftArg,RightArg,SQLOperator,Dict,SQLCompOp).

% ---------------------------------------------------------------------------

:- comment(doinclude,translate_arithmetic_function/5).

:- pred translate_arithmetic_function(Result,Expression,SQLWhere,Dict,NewDict)

# "Arithmetic functions (left side of is/2 operator is bound to value
   of expression on right side) may be called with either:

   @begin{itemize} 
   @item @var{Result} unbound: then @var{Result} is bound to the
   value of the evaluation of @var{Expression},

   @item @var{Result} bound: then an equality condition is returned
   between the value of @var{Result} and the value of the evaluation
   of @var{Expression}.

   Only the equality test shows up in the WHERE clause of an SQLquery.
   @end{itemize}
".

translate_arithmetic_function('$var$'(VarId),Expression,[],Dict,NewDict):-
   % assigment of value of arithmetic expression to variable - does not
   % show up in WHERE-part, but expression corresponding to
   % variable must be stored in Dict for projection translation
   evaluable_expression(Expression,Dict,ArithExpression,Type),
   add_to_dictionary(VarId,is,ArithExpression,Type,all,Dict,NewDict).


translate_arithmetic_function('$var$'(VarId),Expression,
	                      ArithComparison,Dict,Dict):-
   % --- test if left side evaluates to right side: return equality comparison
   % Left side consists of qualified attribute, i.e. range variable must not be
   % arithmetic operator is/2 

   lookup(VarId,Dict,PrevRangeVar,PrevAtt,PrevType),
   \+ (PrevRangeVar = is),

   % test whether type of attribute is numeric - if not, there's no sense in 
   % continuing the translation
   check_type_compatible(PrevType,number),
   
   evaluable_expression(Expression,Dict,ArithExpression,ExprType),
   check_type_compatible(ExprType,number),
   ArithComparison = [comp(att(PrevRangeVar,PrevAtt),'=',ArithExpression)].


translate_arithmetic_function('$var$'(VarId),Expression,
                              ArithComparison,Dict,Dict):-
   % --- test whether left side evals to right side: return equality comparison
   % Left side consists of arithmetic expression, i.e. VarId is stored in Dict
   % as belonging to arithmetic expression which is expressed as 
   % RangeVar-argument of lookup returning is/2. Type information is implicit 
   % through the is/2 functor.

   lookup(VarId,Dict,is,LeftExpr,Type),
   check_type_compatible(Type,number),
   evaluable_expression(Expression,Dict,RightExpr,ExprType),
   check_type_compatible(ExprType,number),
   ArithComparison = [comp(LeftExpr,'=',RightExpr)].


translate_arithmetic_function('$const$'(Constant),Expression,
	                       ArithComparison,Dict,Dict):-
   % --- is/2 used to test whether left side evaluates to right side ---------
   get_type('$const$'(Constant),ConstantType),
   check_type_compatible(ConstantType,number),
   evaluable_expression(Expression,Dict,ArithExpression,ExprType),
   check_type_compatible(ExprType,number),
   ArithComparison = [comp('$const$'(Constant),'=',ArithExpression)].

% ---------------------------------------------------------------------------

:- comment(doinclude,translate_comparison/5).

:- pred translate_comparison(LeftArg,RightArg,CompOp,Dict,SQLComparison) 

# "Translates the left and right arguments of a comparison term into
   the appropriate comparison operation in SQL. The result type of
   each argument expression is checked for type compatibility.".

translate_comparison(LeftArg,RightArg,CompOp,Dict,Comparison):-
   evaluable_expression(LeftArg,Dict,LeftTerm,LeftArgType),
   evaluable_expression(RightArg,Dict,RightTerm,RightArgType),
   check_type_compatible(LeftArgType,RightArgType),
   Comparison = [comp(LeftTerm,CompOp,RightTerm)].

% ---------------------------------------------------------------------------

:- pred translate_functor(Functor,QualifiedTableName) 

# "Translate_functor searches for the matching relation table name for
   a given functor and creates a unique range variable to result in a
   unique qualified relation table name.".

translate_functor(Functor,Arity,rel(TableName,RangeVariable)):-
   relation(Functor,Arity,TableName),
   gensym(rel,RangeVariable).

% ---------------------------------------------------------------------------

:- pred translate_arguments(Arguments,RelTable,ArgPos,Conditions,Dict)

# "Organizes the translation of term arguments. One term argument
   after the other is taken from the list of term arguments until the
   list is exhausted.".

translate_arguments([],_,_,[],Dict,Dict).

translate_arguments([Arg|Args],SQLTable,Position,SQLWhere,Dict,NewDict):-
   translate_argument(Arg,SQLTable,Position,Where,Dict,TmpDict),
   NewPosition is Position + 1,
   translate_arguments(Args,SQLTable,NewPosition,RestWhere,TmpDict,NewDict),
   append(Where,RestWhere,SQLWhere).

% ---------------------------------------------------------------------------

:- pred translate_argument(Argument,RelTable,Position,Condition,Dict)

# "The first occurrence of a variable leads to its associated SQL
   attribute information to be recorded in the Dict. Any further
   occurrence creates an equi-join condition between the current
   attribute and the previously recorded attribute.  Constant
   arguments always translate to equality comparisons between an
   attribute and the constant value.".

translate_argument('$var$'(VarId),rel(SQLTable,RangeVar),Position,
                   [],Dict,NewDict):-
   attribute(Position,SQLTable,Attribute,Type),
   add_to_dictionary(VarId,RangeVar,Attribute,Type,all,Dict,NewDict).

translate_argument('$var$'(VarId),rel(SQLTable,RangeVar),Position,
                   AttComparison,Dict,Dict):-
   % --- Var occurred previously - retrieve first occurrence data from dict
   lookup(VarId,Dict,PrevRangeVar,PrevAtt,PrevType),
   attribute(Position,SQLTable,Attribute,Type),
   check_type_compatible(PrevType,Type),
   AttComparison = [comp(att(RangeVar,Attribute),=,att(PrevRangeVar,PrevAtt))].

translate_argument('$const$'(Constant),rel(SQLTable,RangeVar),Position,
                   ConstComparison,Dict,Dict):-
   % --- Equality comparison of constant value and attribute in table ------
   attribute(Position,SQLTable,Attribute,Type),
   get_type('$const$'(Constant),ConstType),
   check_type_compatible(ConstType,Type),
   ConstComparison = [comp(att(RangeVar,Attribute),=,'$const$'(Constant))].

% ---------------------------------------------------------------------------

:- pred lookup(Key,Dict,Value).

lookup(VarId,Dict,RangeVar,Attribute,Type):-
   member(dict(VarId,RangeVar,Attribute,Type,Quant),Dict),
   (
    Quant = all ->
      true
   ;
      nonvar(RangeVar),
      nonvar(Attribute)
   ).

% ---------------------------------------------------------------------------

:- pred add_to_dictionary(Key,RangeVar,Attribute,Quantifier,Dict,NewDict).

add_to_dictionary(Key,RangeVar,Attribute,Type,_,Dict,Dict):-
   member(dict(Key,RangeVar,Attribute,Type,existential),Dict).

add_to_dictionary(Key,RangeVar,Attribute,Type,Quantifier,Dict,NewDict):-
   \+(member(dict(Key,_,_,_,_),Dict)),
   NewDict = [dict(Key,RangeVar,Attribute,Type,Quantifier)|Dict].

% ---------------------------------------------------------------------------

:- comment(doinclude,aggregate_function/3).

:- pred aggregate_function(AggregateFunctionTerm,Dict,AggregateFunctionQuery)

# "Supports the Prolog aggregate function terms listed in
   @pred{aggregate_functor/2} within arithmetic expressions.
   Aggregate functions are translated to the corresponding SQL
   built-in aggregate functions.".

aggregate_function(AggregateFunctionTerm,Dict,AggregateFunctionExpression):-
   AggregateFunctionTerm =..[AggFunctor,AggVar,AggGoal],
   aggregate_functor(AggFunctor,SQLFunction),
   conjunction(AggGoal,AggConjunction),
   aggregate_query_generation(SQLFunction,AggVar,AggConjunction,Dict,
                              AggregateFunctionExpression).

conjunction(Goal,Conjunction):-
   convert_to_disj_norm_form(Goal,[Conjunction]).


% ---------------------------------------------------------------------------

:- pred aggregate_query_generation(Function,FunctionVariable,AggGoal,Dict,
        AggregateQuery) 

# "Compiles the function variable (representing the attribute over
   which the aggregate function is to be computed) and the aggregate
   goal (representing the selection and join conditions for the
   computation of the aggregate function) to an SQL aggregate function
   subquery.".

aggregate_query_generation(count,'$const$'('*'),AggGoal,Dict,AggregateQuery):-
   translate_conjunction(AggGoal,SQLFrom,SQLWhere,Dict,_TmpDict),

   % ATTENTION! It is assumed that in count(*) aggregate query terms there 
   % cannot be free variables because '*' stands for "all arguments"

   AggregateQuery = 
      agg_query(_Function,(count,['$const$'(*)]),SQLFrom,SQLWhere,[]).


aggregate_query_generation(Function,FunctionVariable,AggGoal,Dict,
                           AggregateQuery):-
   translate_conjunction(AggGoal,SQLFrom,SQLWhere,Dict,TmpDict),

   % --- only vars occurring in the aggregate goal are relevant to the 
   % translation of the function variable and the free variables in the goal.
   % Thus subtract from TmpDict all entries of Dict
   set_difference(TmpDict,Dict,AggDict),
 
   translate_projection(FunctionVariable,AggDict,SQLSelect),
   translate_grouping(FunctionVariable,AggDict,SQLGroup),
   AggregateQuery = agg_query(Function,SQLSelect,SQLFrom,SQLWhere,SQLGroup).


% ---------------------------------------------------------------------------

:- pred translate_grouping(FunctionVariable,Dict,SQLGroup) 

# "Finds the free variables in the aggregate function term and collects their
   corresponding SQL qualified attributes in the SQLGroup list.".

translate_grouping(FunctionVariable,Dict,SQLGroup):-
   free_vars(FunctionVariable,Dict,FreeVariables),
   translate_free_vars(FreeVariables,SQLGroup).

% ---------------------------------------------------------------------------

:- pred free_vars(FunctionVariable,Dict,FreeVarList)

# "A Variable is free if it neither occurs as the FunctionVariable,
   nor is stored as existentially quantified (through ^/2 in the
   original goal) in the dictionary. FreeVars contains for each
   variable the relevant attribute and relation information contained
   in the dictionary.".

free_vars(FunctionVariable,Dict,FreeVarList):-
  projection_term_variables(FunctionVariable,FunctionVariableList),
  findall((Var,Table,Attribute),
      (member(dict(Var,Table,Attribute,_Type,all),Dict),
       \+(member(dict(Var,_,_,_,_),FunctionVariableList))
      ),
      FreeVarList).


% ---------------------------------------------------------------------------

:- pred function_variable_list(FunctionVariable,FunctionVariableList)

# "Extracts the list of variables which occur in the function variable term.

   @bf{Note:} @var{FunctionVariable} may only contain one single variable.".
 

function_variable_list('$var$'(VarId),[VarId]).


% ---------------------------------------------------------------------------

:- pred translate_free_vars(FreeVars,SQLGroup)

# "Translates dictionary information on free variables to SQLGroup of
   aggregate function query.".

translate_free_vars([],[]).
translate_free_vars([(_VarId,Table,Attribute)|FreeVars],
                    [att(Table,Attribute)|SQLGroups]):-
   translate_free_vars(FreeVars,SQLGroups).

% ---------------------------------------------------------------------------

:- pred evaluable_expression(ExpressionTerm,Dictionary,Expression,Type)

# "Constructs SQL arithmetic expressions with qualified attribute
   names from the Prolog arithmetic expression term and the
   information stored in the dictionary.

   The type of an evaluable function is returned in the argument @var{Type}.

   The dictionary is not changed because it is used for lookup only.".
 
evaluable_expression(AggregateFunctionTerm,Dictionary,
                     AggregateFunctionExpression,number):-
   aggregate_function(AggregateFunctionTerm,Dictionary,
                      AggregateFunctionExpression).

%% MH Unified all operators here. Also using type unions (previously was 
%% hardwired to 'number'). 
evaluable_expression(ArithExpr,Dictionary,SQLArithExpr,EType):-
   ArithExpr =.. [PrologOp,LeftExp,RightExp],
   arithmetic_functor(PrologOp,SQLOp),
   evaluable_expression(LeftExp,Dictionary,LeftAr,LType),
   evaluable_expression(RightExp,Dictionary,RightAr,RType),
   check_type_union(LType,RType,EType),
   SQLArithExpr =.. [SQLOp,LeftAr,RightAr].

evaluable_expression('$var$'(VarId),Dictionary,att(RangeVar,Attribute),Type):-
   lookup(VarId,Dictionary,RangeVar,Attribute,Type),
   RangeVar \== is.

evaluable_expression('$var$'(VarId),Dictionary,ArithmeticExpression,Type):-
   lookup(VarId,Dictionary,is,ArithmeticExpression,Type).

evaluable_expression('$const$'(Const),_,'$const$'(Const),ConstType):-
   get_type('$const$'(Const),ConstType).


% ----------------------------------------------------------------------------
% Pretty printing of queries
% ----------------------------------------------------------------------------

:- pred printqueries(SQLTermList) :: list(sqlterm)

   # "Print to standard output in SQL the list of SQL term @var{SQLTermList}.".

printqueries([Query]):-
   nl,
   print_query(Query),
   write(';'),
   nl,
   nl.

printqueries([Query|Queries]):-
   \+ (Queries = []),
   nl,
   print_query(Query),
   nl,
   write('UNION'),
   nl,
   printqueries(Queries).


% ----------------------------------------------------------------------------

:- pred print_query(QueryCode). 

print_query(query([agg_query(Function,Select,From,Where,Group)],_,_)):-
   % --- ugly rule here: aggregate function only in SELECT Part of query ----
   !,
   print_query(agg_query(Function,Select,From,Where,Group)).

print_query(query(Select,From,Where)):-
   print_clause3('SELECT',Select,','),
   nl,
   print_clause3('FROM',From,','),
   nl,
   print_clause3('WHERE',Where,'AND'),
   nl.

print_query(agg_query(Function,Select,From,Where,Group)):-
   print_clause4('SELECT',Function,Select,','),
   nl,
   print_clause3('FROM',From,','),
   nl,
   print_clause3('WHERE',Where,'AND'),
   nl,
   print_clause3('GROUP BY',Group,',').

print_query(negated_existential_subquery(Select,From,Where)):-
   write('NOT EXISTS'),
   nl,
   write('('),
   print_clause3('SELECT',Select,','),
   nl,
   print_clause3('FROM',From,','),
   nl,
   print_clause3('WHERE',Where,'AND'),
   nl,
   write(')').


% ----------------------------------------------------------------------------

:- pred print_clause4(Keyword,Function,ClauseCode,Separator) 

  # "Where 

   @var{Keyword} is one of SELECT, FROM, WHERE, or GROUP BY, 

   @var{Function} is an aggregation function, and

   @var{ClauseCode} is the code corresponding to the appropriate clause
   of an SQL query, and

   @var{Separator} indicates the character(s) which separate the items
   of a clause from each other (',' or 'AND').".

print_clause4(Keyword,Function,[Column],Separator):-
   write(Keyword),
   write(' '),
   write(Function),
   write('('),
   print_clause2([Column],Separator),
   write(')').

% ----------------------------------------------------------------------------

:- pred print_clause3(Keyword,ClauseCode,Separator).

print_clause3(_Keyword,[],_).
print_clause3(Keyword,[Column|RestColumns],Separator):-
   write(Keyword),
   write(' '),
   print_clause2([Column|RestColumns],Separator).

% ----------------------------------------------------------------------------

:- pred print_clause2(ClauseCode,Separator).

print_clause2([Item],_):-
   print_column(Item).
print_clause2([Item,NextItem|RestItems],Separator):-
   print_column(Item),
   write(' '),
   write(Separator),
   write(' '),
   print_clause2([NextItem|RestItems],Separator).


% ----------------------------------------------------------------------------

:- pred print_column(ColumnCode).

print_column('*'):-
   write('*').
print_column(att(RangeVar,Attribute)):-
   write(RangeVar),
   write('.'),
   write(Attribute).
print_column(rel(Relation,RangeVar)):-
   write(Relation),
   write(' '),
   write(RangeVar).
print_column('$const$'(String)):-
   get_type('$const$'(String),string),
   writeq(String). 
   %% Using writeq ^^^^^^^^^^  
print_column('$const$'(Number)):-
   get_type('$const$'(Number),NumType),
   check_type_compatible(NumType,number),
   write(Number).
print_column(comp(LeftArg,Operator,RightArg)):-
   print_column(LeftArg),
   write(' '),
   write(Operator),
   write(' '),
   print_column(RightArg).
print_column(LeftExpr * RightExpr):-
   print_column(LeftExpr),
   write('*'),
   print_column(RightExpr).
print_column(LeftExpr / RightExpr):-
   print_column(LeftExpr),
   write('/'),
   print_column(RightExpr).
print_column(LeftExpr + RightExpr):-
   print_column(LeftExpr),
   write('+'),
   print_column(RightExpr).
print_column(LeftExpr - RightExpr):-
   print_column(LeftExpr),
   write('-'),
   print_column(RightExpr).
print_column(agg_query(Function,Select,From,Where,Group)):-
   nl,
   write('('),
   print_query(agg_query(Function,Select,From,Where,Group)),
   write(')').
print_column(negated_existential_subquery(Select,From,Where)):-
   print_query(negated_existential_subquery(Select,From,Where)).

% ----------------------------------------------------------------------------
% Building a string containing the query
% ----------------------------------------------------------------------------

:- pred buildqueries(SQLTermList,SQLString) :: list(sqlterm) * string

   # "Builds a SQL string from the list of SQL term @var{SQLTermList}.".

buildqueries(QueriesList,String):-
   buildqueries2(QueriesList,[],String).

buildqueries2([Query],PreviousString,QueryString):-
   build_query(Query,QueryStr),
   list_concat([PreviousString," ",QueryStr,";"],QueryString).

buildqueries2([Query|Queries],PreviousString,QueryString):-
   \+ (Queries = []),
   build_query(Query,QueryStr1),
   list_concat([PreviousString," ",QueryStr1," UNION "],QueryString1),
   buildqueries2(Queries, QueryString1, QueryString).

% ----------------------------------------------------------------------------

:- pred build_query(QueryCode,SQLString). 

build_query(query([agg_query(Function,Select,From,Where,Group)],_,_),SQLStr):-
   % --- ugly rule here: aggregate function only in SELECT Part of query ----
   !,
   build_query(agg_query(Function,Select,From,Where,Group),SQLStr).

build_query(query(Select,From,Where),SQLStr):-
   build_clause3("SELECT",Select,",",Str1),
   build_clause3("FROM",From,",",Str2),
   build_clause3("WHERE",Where,"AND",Str3),
   list_concat([Str1," ",Str2," ",Str3],SQLStr).


build_query(agg_query(Function,Select,From,Where,Group),SQLStr):-
   build_clause4("SELECT",Function,Select,",",Str1),
   build_clause3("FROM",From,",",Str2),
   build_clause3("WHERE",Where,"AND",Str3),
   build_clause3("GROUP BY",Group,",",Str4),
   list_concat([Str1," ",Str2," ",Str3," ",Str4],SQLStr).

build_query(negated_existential_subquery(Select,From,Where),SQLStr):-
   build_clause3("SELECT",Select,",",Str1),
   build_clause3("FROM",From,",",Str2),
   build_clause3("WHERE",Where,"AND",Str3),
   list_concat(["NOT EXISTS ","(",Str1," ",Str2," ",Str3,")"],SQLStr).

% ----------------------------------------------------------------------------

:- pred build_clause4(Keyword,Function,ClauseCode,Separator,SQLString)

   # "Where 

   @var{Keyword} is one of SELECT, FROM, WHERE, or GROUP BY, 

   @var{Function} is an aggregation function, and

   @var{ClauseCode} is the code corresponding to the appropriate clause
   of an SQL query, and

   @var{Separator} indicates the character(s) which separate the items
   of a clause from each other (, or AND), and

   @var{SQLString} contains the SQL sentence fragment corresponding to the
   clause.".

build_clause4(Keyword,Function,[Column],Separator,SQLString):-
   build_clause2([Column],Separator,Str),
%%   aggregate_functor(Function,FunctionTerm), modified 19-10-98, Ignacio
   atom_codes(Function,FunctionString),
   list_concat([Keyword," ",FunctionString,"(",Str,")"],SQLString).

% ----------------------------------------------------------------------------

:- pred build_clause3(Keyword,ClauseCode,Separator,SQLString).

build_clause3(_Keyword,[],_,"").
build_clause3(Keyword,[Column|RestColumns],Separator,SQLString):-
   build_clause2([Column|RestColumns],Separator,Str),
   list_concat([Keyword," ",Str],SQLString).

% ----------------------------------------------------------------------------

:- pred build_clause2(ClauseCode,Separator,SQLString).

build_clause2([Item],_,SQLString):-
   build_column(Item,SQLString).
build_clause2([Item,NextItem|RestItems],Separator,SQLString):-
   build_column(Item,Str1),
   build_clause2([NextItem|RestItems],Separator,Str2),
   list_concat([Str1," ",Separator," ",Str2],SQLString).

% ----------------------------------------------------------------------------

:- pred build_column(ColumnCode,SQLString).

build_column('*',"*").
build_column(att(RangeVar,Attribute),SQLString):-
   atom_codes(RangeVar,RangeVarStr),
   atom_codes(Attribute,AttributeStr),
   list_concat([RangeVarStr,".",AttributeStr],SQLString).
build_column(rel(Relation,RangeVar),SQLString):-
   atom_codes(Relation,RelationStr),
   atom_codes(RangeVar,RangeVarStr),
   list_concat([RelationStr," ",RangeVarStr],SQLString).

build_column('$const$'(StringAtom),String):-
   get_type('$const$'(StringAtom),string),
   atom_codes(StringAtom,String).
%%%   writeq(String).    %% Using writeq ^^^^^^^^^^  
build_column('$const$'(Number),String):-
   get_type('$const$'(Number),NumType),
   check_type_compatible(NumType,number),
   number_codes(Number,String). 
build_column(comp(LeftArg,Operator,RightArg),String):-
   build_column(LeftArg,Str1),
   atom_codes(Operator,Str2),
   build_column(RightArg,Str3),
   list_concat([Str1," ",Str2," ",Str3],String).
build_column(LeftExpr * RightExpr,String):-
   build_column(LeftExpr,Str1),
   build_column(RightExpr,Str2),
   list_concat([Str1,"*",Str2],String).
build_column(LeftExpr / RightExpr,String):-
   build_column(LeftExpr,Str1),
   build_column(RightExpr,Str2),
   list_concat([Str1,"/",Str2],String).
build_column(LeftExpr + RightExpr,String):-
   build_column(LeftExpr,Str1),
   build_column(RightExpr,Str2),
   list_concat([Str1,"+",Str2],String).
build_column(LeftExpr - RightExpr,String):-
   build_column(LeftExpr,Str1),
   build_column(RightExpr,Str2),
   list_concat([Str1,"-",Str2],String).
build_column(agg_query(Function,Select,From,Where,Group),String):-
%%   nl,
   build_query(agg_query(Function,Select,From,Where,Group),Str),
   list_concat(["(",Str,")"],String).
build_column(negated_existential_subquery(Select,From,Where),String):-
   build_query(negated_existential_subquery(Select,From,Where),String).


% ----------------------------------------------------------------------------
% Conversion of SQL term to string
% ----------------------------------------------------------------------------

:- pred sqlterm2string(+Queries,-QueryString) :: list(sqlterm) * sqlstring

# "@var{QueryString} is a string representation of the list of
   queries in Prolog-term format in @var{Queries}.".

sqlterm2string(SQLQueryTerm,SQLQueryString) :-
      buildqueries(SQLQueryTerm,SQLQueryString),
      !,
      debug_message(" Translation done ",[]).
sqlterm2string(SQLQueryTerm,_) :-
    error_message("could not convert to string ~w",[SQLQueryTerm]),
    fail.

%% %% Original code fails on some SQLQueryTerms for which printqueries/1 succeeds!
%% %% Doing this serious kludge instead for now:
%%             REPAIRED! 
%% sqlterm2string(SQLQueryTerm,SQLQueryString) :-
%%    telling(O),
%%    TMP = '/tmp/sqlqueryfileIgnacio',
%%    tell(TMP),
%%    write('"'),
%%    printqueries(SQLQueryTerm),
%%    write('"'),
%%    write('.'),
%%    nl,
%%    told,
%%    tell(O),
%%    seeing(I),
%%    see(TMP),
%%    do_the_read(SQLQueryTerm,I,SQLQueryString).
%% 
%% do_the_read(_SQLQueryTerm,I,SQLQueryString)  :-
%%    read(SQLQueryString),
%%    seen,
%%    see(I),
%%    !.
%% do_the_read(SQLQueryTerm,I,_SQLQueryString)  :-
%%    error_message("could not convert to string ~w",[SQLQueryTerm]),
%%    seen,
%%    see(I),
%%    fail.

%% sqlterm2string(Queries,QueryString) :-
%%    queries_dstring(Queries,QueryString,[]),
%%    !.
%% sqlterm2string(SQLQueryTerm,_SQLQueryString) :-
%%    error_message("could not convert to string ~w",[SQLQueryTerm]).


queries_dstring([Query],QueryString,Diff):-
   query_dstring(Query,QueryString,Diff).
queries_dstring([Query|Queries],QueryString,Diff):-
   Queries \== [],
   query_dstring(Query,QueryString,X1),
   column_dstring('UNION',X1,X2),
   queries_dstring(Queries,X2,Diff).

query_dstring(query([agg_query(Function,Select,From,Where,Group)],_,_),
	     QueryString,Diff):-
   % --- ugly rule here: aggregate function only in SELECT Part of query ----
   !,
   query_dstring(agg_query(Function,Select,From,Where,Group),QueryString,Diff).
query_dstring(query(Select,From,Where),QueryString,Diff):-
   clause_dstring5('SELECT',Select,',',QueryString,X1),
   clause_dstring5('FROM',From,',',X1,X2),
   clause_dstring5('WHERE',Where,'AND',X2,Diff).
query_dstring(agg_query(Function,Select,From,Where,Group),QueryString,Diff):-
   clause_dstring6('SELECT',Function,Select,',',QueryString,X1),
   clause_dstring5('FROM',From,',',X1,X2),
   clause_dstring5('WHERE',Where,'AND',X2,X3),
   clause_dstring5('GROUP BY',Group,',',X3,Diff).
query_dstring(negated_existential_subquery(Select,From,Where),QueryString,Diff):-
   column_dstring('NOT EXISTS(',QueryString,X1),   
   clause_dstring5('SELECT',Select,',',X1,X2),
   clause_dstring5('FROM',From,',',X2,X3),
   clause_dstring5('WHERE',Where,'AND',X3,X4),
   column_dstring(')',X4,Diff).

% ----------------------------------------------------------------------------

:- pred clause_dstring5(Keyword,ClauseCode,Junctor,CurrAtom,Diff)

# "Where 

   @var{Keyword} is one of SELECT, FROM, WHERE, or GROUP BY, 

   @var{ClauseCode} is the code corresponding to the appropriate
   clause of an SQL query, and

   @var{Junctor} indicates the character(s) through which the items of
   a clause are separated from each other (',' or 'AND').".

clause_dstring5(_Keyword,[],_,QueryString,QueryString).
clause_dstring5(Keyword,[Column|RestColumns],Junctor,QueryString,Diff):-
   column_dstring(Keyword,QueryString,X1),
   column_dstring(' ',X1,X2),
   clause_dstring4([Column|RestColumns],Junctor,X2,X3),
   column_dstring(' ',X3,Diff).

:- pred clause_dstring6(Keyword,ClauseCode,Junctor,CurrAtom,QueryAtom,Diff).

clause_dstring6(Keyword,Function,[Column],Junctor,QueryString,Diff):-
   column_dstring(Keyword,QueryString,X1),
   column_dstring(' ',X1,X2),
   column_dstring(Function,X2,X3),
   column_dstring('(',X3,X4),
   clause_dstring4([Column],Junctor,X4,X5),
   column_dstring(') ',X5,Diff).

:- pred clause_dstring/4.

clause_dstring4([Item],_,QueryString,Diff):-
   column_dstring(Item,QueryString,Diff).
clause_dstring4([Item,NextItem|RestItems],Junctor,QueryString,Diff):-
   column_dstring(Item,QueryString,X1),
   column_dstring(' ',X1,X2),
   column_dstring(Junctor,X2,X3),
   column_dstring(' ',X3,X4),
   clause_dstring4([NextItem|RestItems],Junctor,X4,Diff).

:- pred column_dstring/3.

column_dstring(att(RangeVar,Attribute),QueryString,Diff):-
   column_dstring(RangeVar,QueryString,X1),
   column_dstring('.',X1,X2),
   column_dstring(Attribute,X2,Diff).
column_dstring(rel(Relation,RangeVar),QueryString,Diff):-
   column_dstring(Relation,QueryString,X1),
   column_dstring(' ',X1,X2),
   column_dstring(RangeVar,X2,Diff).
column_dstring('$const$'(String),QueryString,Diff):-
   get_type('$const$'(String),string),
   column_dstring('"',QueryString,X1),
   column_dstring(String,X1,X2),
   column_dstring('"',X2,Diff).
column_dstring('$const$'(Number),QueryString,Diff):-
   get_type('$const$'(Number),NumType),
   check_type_compatible(NumType,number),
   column_dstring(Number,QueryString,Diff).
column_dstring(comp(LeftArg,Operator,RightArg),QueryString,Diff):-
   column_dstring(LeftArg,QueryString,X1),
   column_dstring(' ',X1,X2),
   column_dstring(Operator,X2,X3),
   column_dstring(' ',X3,X4),
   column_dstring(RightArg,X4,Diff).
column_dstring(LeftExpr * RightExpr,QueryString,Diff):-
   column_dstring(LeftExpr,QueryString,X1),
   column_dstring('*',X1,X2),
   column_dstring(RightExpr,X2,Diff).
column_dstring(LeftExpr + RightExpr,QueryString,Diff):-
   column_dstring(LeftExpr,QueryString,X1),
   column_dstring('+',X1,X2),
   column_dstring(RightExpr,X2,Diff).
column_dstring(LeftExpr - RightExpr,QueryString,Diff):-
   column_dstring(LeftExpr,QueryString,X1),
   column_dstring('-',X1,X2),
   column_dstring(RightExpr,X2,Diff).
column_dstring(LeftExpr / RightExpr,QueryString,Diff):-
   column_dstring(LeftExpr,QueryString,X1),
   column_dstring('/',X1,X2),
   column_dstring(RightExpr,X2,Diff).
column_dstring(agg_query(Function,Select,From,Where,Group),QueryString,Diff):-
   column_dstring('(',QueryString,X1),
   query_dstring(agg_query(Function,Select,From,Where,Group),X1,X2),
   column_dstring(')',X2,Diff).
column_dstring(negated_existential_subquery(Select,From,Where),
	       QueryString,Diff):-
   query_dstring(negated_existential_subquery(Select,From,Where),
                 QueryString,Diff).
column_dstring(Atom,List,Diff):-
   atom(Atom),
   name(Atom,X1),
   append(X1,Diff,List).

% ----------------------------------------------------------------------------
% ciao version for init_gensym and gensym
% ----------------------------------------------------------------------------

:- pred concat_pred_num(Name,Number,Conc_Pred).

concat_pred_num(Name,Number,Conc_Pred):-
    name(Name,String_1),
    name(Number,String_2),
    append(String_1,String_2,Concatenation),
    name(Conc_Pred,Concatenation).

:- pred variable/1 # "Stores the current variable number.".

:- data variable/1.

:- pred rel/1 # "Stores the current relation number.".

:- data rel/1.

:- pred init_gensym(Root).

init_gensym(Root):-
    New =.. [Root,0],
    set_fact(New).

:- pred gensym(Root,Symbol).

gensym(Root,Symbol):-
    var(Symbol),
    Old =.. [Root,Counter],
    Old,
    NewCounter is Counter+1,
    concat_pred_num(Root,NewCounter,Symbol),
    New =.. [Root,NewCounter],
    set_fact(New).

% ----------------------------------------------------------------------------
% auxiliary predicates (most changed to use built-ins...)
% ----------------------------------------------------------------------------


:- pred set_difference(SetA,SetB,Difference) 

# "@var{Difference} = @var{SetA} - @var{SetB}.".

set_difference([],_,[]).

set_difference([Element|RestSet],Set,[Element|RestDifference]):-
   \+(member(Element,Set)),
   set_difference(RestSet,Set,RestDifference).

set_difference([Element|RestSet],Set,RestDifference):-
   member(Element,Set),
   set_difference(RestSet,Set,RestDifference).


% ----------------------------------------------------------------------------
% Mapping of Prolog operators to SQL operators
% ----------------------------------------------------------------------------

:- comment(doinclude,comparison/2).

:- pred comparison(PrologOperator,SQLOperator) :: atm * atm

# "Defines the mapping between Prolog operators and SQL operators:
@includedef{comparison/2}".

comparison(=,=).
comparison(<,<).
comparison(>,>).
comparison(@<,<).
comparison(@>,>).


:- comment(doinclude,negated_comparison/2).

:- pred negated_comparison(PrologOperator,SQLOperator) :: atm * atm

# "Defines the mapping between Prolog operators and the complementary
   SQL operators: 
@includedef{negated_comparison/2}".

negated_comparison(=,'<>').
negated_comparison(\==,=).
negated_comparison(>,=<).
negated_comparison(=<,>).
negated_comparison(<,>=).
negated_comparison(>=,<).

:- comment(doinclude,arithmetic_functor/2).

:- pred arithmetic_functor(PrologFunctor,SQLFunction) :: atm * atm

# "Defines the admissible arithmetic functions on the Prolog side and
   their correspondence on the SQL side:
@includedef{arithmetic_functor/2}".

arithmetic_functor(+,+).
arithmetic_functor(-,-).
arithmetic_functor(*,*).
arithmetic_functor(/,/).

:- comment(doinclude,aggregate_functor/2).

:- pred aggregate_functor(PrologFunctor,SQLFunction) :: atm * atm

# "Defines the admissible aggregate functions on the Prolog side and
   their correspondence on the SQL side:
@includedef{aggregate_functor/2}".

aggregate_functor(avg,'AVG').
aggregate_functor(min,'MIN').
aggregate_functor(max,'MAX').
aggregate_functor(sum,'SUM').
aggregate_functor(count,'COUNT').

% ----------------------------------------------------------------------------
% Type system
% ----------------------------------------------------------------------------

check_type_union(TypeA,TypeB,TypeC) :- 
	type_union(TypeA,TypeB,TypeC),
	!.
check_type_union(TypeA,TypeB,_TypeC) :- 
	error_message("incompatible types ~w, ~w",[TypeA,TypeB]),
	fail.

:- pred type_union(TypeA,TypeB,Union) :: sqltype * sqltype * sqltype

# "@var{Union} is the union type of @var{TypeA} and @var{TypeB}.".

type_union(TypeA,TypeA,TypeA).
type_union(TypeA,TypeB,TypeA) :- 
	subtype(TypeB,TypeA).
type_union(TypeA,TypeB,TypeB) :- 
	subtype(TypeA,TypeB).
type_union(TypeA,TypeB,TypeC) :- 
	subtype(TypeA,TypeC),
	subtype(TypeB,TypeC).


check_type_compatible(TypeA,TypeB) :- 
	type_compatible(TypeA,TypeB),
	!.
check_type_compatible(TypeA,TypeB) :- 
	error_message("incompatible types ~w, ~w",[TypeA,TypeB]),
	fail.

:- pred type_compatible(TypeA,TypeB) :: sqltype * sqltype

# "Checks if @var{TypeA} and @var{TypeB} are compatible types, i.e.,
   they are the same or one is a subtype of the other.".

type_compatible(Type,Type):-
   sqltype(Type).
type_compatible(SubType,Type):-
   subtype(SubType,Type).
type_compatible(Type,SubType):-
   subtype(SubType,Type).


:- pred subtype(SubType,SuperType) :: sqltype * sqltype

#  "Simple type hierarchy checking.".

subtype(SubType,SuperType):-
   is_subtype(SubType,SuperType).
subtype(SubType,SuperType):-
   is_subtype(SubType,InterType),
   subtype(InterType,SuperType).

:- prop sqltype(Type)  # "@var{Type} is an SQL data type supported by the translator.".

%% Should be ordered from most concrete to most general.
sqltype(natural).
sqltype(integer).
sqltype(real).
sqltype(number).
sqltype(string).

:- comment(sqltype/1,"@includedef{sqltype/1}

   These types have the same meaning as the corresponding standard types
   in the @lib{basictypes} library.").

:- pred is_subtype(SubType,SuperType) :: sqltype * sqltype

# "Simple type hierarchy for numeric types.".

is_subtype(integer,number).
is_subtype(real,number).
is_subtype(natural,integer).


:- pred get_type(+Constant,Type) :: term * sqltype

# "Prolog implementation specific definition of type retrievals. CIAO 
   Prolog version given here.".

get_type('$const$'(Constant),number):-
   number(Constant).
get_type('$const$'(Constant),real):-
   float(Constant).
get_type('$const$'(Constant),integer):-
   integer(Constant).
get_type('$const$'(Constant),string):-
   atom(Constant).
