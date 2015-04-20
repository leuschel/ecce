
% Utilities

% Note: keep the code of member_term this code here (not use member_0
% of dlists.pl), since it is intended all this code be self contained
% in order to be included somewhere else

member_term(X, [Y|_]) :- X == Y, !.
member_term(X, [_|L]) :- member_term(X, L).

% Flattens a disjunction of items.

flat_disjunction(((A;B);C), FlatBody):- 
       !,
       flat_disjunction(A, FA),
       flat_disjunction(B, FB),
       flat_disjunction(C, FC), 
       append_disjunction(FB, FC, AG),
       append_disjunction(FA, AG, FlatBody).
flat_disjunction((Lit;Body), (Lit;FlatBody)):- 
       !,
       flat_disjunction(Body, FlatBody).
flat_disjunction(Lit, Lit).

% Appends two disjunctions of items

append_disjunction((Lit;Body), Goals, (Lit;AppGoals)):- 
        !,
	append_disjunction(Body, Goals, AppGoals).
append_disjunction(Lit, Goals, (Lit;Goals)).

%

% Translation from typedef to clauses.
 
:- pred typedef_to_pred(+Def, +Pred, -Cls)

=> external_type_disjunction * atm * clause_list 

# "Translates a type defined in external rule format (@var{Def}) into
   predicate format, and put the clauses defining the predicate (named
   @var{Pred}) in @var{Cls}.".

typedef_to_pred((Or; Ors), Pred, [Clause|Cls]):- 
        !,
        one_disjunct_to_clause(Or, Pred, Clause),
	typedef_to_pred(Ors, Pred, Cls).
typedef_to_pred(Or, Pred, [Clause]):-
        one_disjunct_to_clause(Or, Pred, Clause).

one_disjunct_to_clause(Or, Pred, clause(G, Body)):-
        copy_term((Or,Pred), (CopOr,CopPred)), 
        create_param_type_call(CopPred, X, G),
	typedef_to_pred0(CopOr, X, Body).

typedef_to_pred0(Or, X, Body):-
        typedef_to_pred1(Or, X, Body1),
        (var(Body1) -> Body = true ; Body = Body1).

typedef_to_pred1(Type, CType, _):- 
        constant_symbol(Type, CType), 
        !.
typedef_to_pred1(Term, X, Body):- 
        atom(Term),
        \+ Term = [],
        !,
        Body=..[Term, X].
typedef_to_pred1(Term, X, Body):- 
        par_rule_type_symbol(Term),
        !,
        create_param_type_call(Term, X, Body).
typedef_to_pred1(Type, X, Body):-
        compound_pure_type_term(Type, Term, Name, Arity),
        !,
        functor(X, Name, Arity),
        typedef_to_pred_compound(1, Arity, Term, X, Body).
typedef_to_pred1(Term, X, Body):- 
        var(Term),
        !,
        Body = regtype(X, Term). 

create_param_type_call(ParType, Arg, TypeCall):-
        ParType =.. [Name|TypeArgs],
        TypeCall =.. [Name, Arg|TypeArgs].

 %% create_param_type_call(ParType, Arg, TypeCall):-
 %%    nonvar(ParType),
 %%    functor(ParType, Name, Arity),
 %%    Arity1 is Arity + 1,
 %%    definable_type(Name/Arity1, Error),
 %%    var(Error), 
 %%    ParType =.. [Name|ParametricVars],
 %%    list_of_unique_variables(ParametricVars, Name/Arity1, Error),
 %%    var(Error), 
 %%    TypeCall =.. [Name, Arg|TypeArgs].

typedef_to_pred_compound(N, A, Term, X, Lit):- 
        N = A, !,
        arg(N, Term, ArgT),
	arg(N, X, ArgX),
	typedef_to_pred1(ArgT, ArgX, Lit).
typedef_to_pred_compound(N, A, Term, X, Body):-
	N < A, 
        !,
        arg(N, Term, ArgT),
	arg(N, X, ArgX),
	typedef_to_pred1(ArgT, ArgX, Lit),
        (nonvar(Lit) -> 
            Body = (Lit, Body1)
            ;
            Body = Body1),
	N1 is N + 1,
	typedef_to_pred_compound(N1, A, Term, X, Body1).

% End of translation from typedef to clauses.


% Validation of regular types in rule format 

:- pred valid_regular_type_rule_no_mess(R)

# "@var{R} is a valid @tt{regular type rule}. It does not report any error message.".

valid_regular_type_rule_no_mess(R):-
   valid_regular_type_rule_e(R, Error), 
   var(Error). 

:- pred valid_regular_type_rule_e(+R, -Error)

# "Checks whether @var{R} is a valid @tt{regular type rule}.  If it
   is, then the predicate succeeds and @var{Error} is an unbound
   variable.  Otherwise, either, the predicate fails, or succeeds with
   @var{Error} bound to an error message.".

valid_regular_type_rule_e(typedef( '::='( Symbol, Def) ), Error):-
     valid_rule_type_symbol(Symbol, ParametricVars, Error),
     flat_disjunction(Def, FlatDef),
     valid_type_disjunction(FlatDef, ParametricVars). 

:- pred valid_type_disjunction(X, ParametricVars) 
  
# "@var{X} is a disjunction of valid regular types.".

valid_type_disjunction((Type;Body), ParametricVars):-
     !,
     regular_type_expression(Type, ParametricVars),
     valid_type_disjunction(Body, ParametricVars).
valid_type_disjunction(Type, ParametricVars):-
     regular_type_expression(Type, ParametricVars).
 
:- pred regular_type_expression(X, Vars)

  # "@var{X} is a regular type.".

regular_type_expression(Type, Vars):-
   rule_type_symbol_instance(Type, Vars); 
   constant_symbol(Type, _);
   top_type(Type);
   bot_type(Type);
   base_type_symbol(Type);
   compound_type_term_expression(Type, Vars);
   parametric_type_variable(Type, Vars);
   struct_type(Type). %% Added by PLG 23 Oct 98

parametric_type_variable(Type, Vars):-
   var(Type),
   member_term(Type, Vars).

compound_type_term_expression(Type, Vars):-
   compound_pure_type_term(Type, Comp, _Name, Arity),
   compound_term_exp(Arity, Comp, Vars).

compound_term_exp(0, _Type, _Vars):-
  !. 
compound_term_exp(A, Type, Vars):-
  A > 0,
  arg(A, Type, Arg),
  regular_type_expression(Arg, Vars),
  A1 is A - 1,
  compound_term_exp(A1, Type, Vars). 

:- pred rule_type_symbol_instance(+Type, +ParametricVars) 

# "@var{Type} is a @tt{(possibly parametric) type symbol instance}.".

rule_type_symbol_instance(Type, ParametricVars):- 
   rule_type_symbol(Type),
   Type =.. [_Name|Parameters],
   valid_parameters_in_type(Parameters, ParametricVars).

valid_parameters_in_type([Param|Parameters], ParametricVars):-
   regular_type_expression(Param, ParametricVars),
   valid_parameters_in_type(Parameters, ParametricVars).
valid_parameters_in_type([], _ParametricVars).

:- pred valid_rule_type_symbol(+Type, -ParametricVars)

# "@var{Type} is a @tt{(possibly parametric) rule type symbol} with
   parametric variables @var{ParametricVars}.".

valid_rule_type_symbol(Type, ParametricVars, Error):- 
   nonvar(Type),
   functor(Type, Name, Arity),
   (rule_type_symbol(Type) -> 
       Type =.. [Name|ParametricVars],
       list_of_unique_parametric_variables(ParametricVars, Name/Arity, Error)
       ;
       Error = forbidden_pred_defined_as_type(Name/Arity)).

list_of_unique_parametric_variables(_ParametricVars, _Pred, Error):-
    nonvar(Error), !.
list_of_unique_parametric_variables(ParametricVars, _Pred, _Error):-
    list_of_unique_variables_2(ParametricVars, []), !.
list_of_unique_parametric_variables(ParametricVars, Pred, Error):-
    Error = bad_parametric_vars(ParametricVars, Pred).

list_of_unique_variables_2([], _).
list_of_unique_variables_2([Var|VarList], Seen):-
   var(Var),
   \+ member_term(Var, Seen),
   list_of_unique_variables_2(VarList, [Var|Seen]).

% End of validation of regular types in rule format 
