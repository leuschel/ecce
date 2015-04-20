% Complementary definitions to those in basic_type_operations.pl

number_constant(Type, Type):- 
   number(Type).
number_constant(Type, Term):- 
   nonvar(Type),
   Type = ^(Term),
   number(Term).

atom_constant(Type, Type):-
   nonvar(Type), 
   Type = [], 
   !.
atom_constant(Type, Term):- 
   nonvar(Type),
   Type = ^(Term),
   atom(Term).

:- pred constant_symbol(Type, CType)

# "@var{Type} is the type representation of the @tt{constant} @var{CType}
   (numeric or non-numeric).".

constant_symbol(Type, CType):- 
   number_constant(Type, CType); atom_constant(Type, CType).


top_type(X):- X == term.
bot_type(X):- X == bot.
ground_type(X):- X == gnd.
numeric_type(X):- X == num.
atom_type(X):- X == atm.
var_type(X):- X == var.


:- pred base_type_symbol(X)

# "@var{X} is a @tt{base type symbol}. Different base type symbols must
define disjoint sets of terms.".

base_type_symbol(X):- 
     numeric_type(X); 
     atom_type(X);
     var_type(X).

:- pred compound_pure_type_term(+Type, -Comp, -Name, -Arity)

# "@var{Type} represents a pure type term with main functor 
  @var{Name}  of
  arity greater than zero @var{Arity}. 
  @var{Comp} is a compound term with  main functor 
  @var{Name}  of
  arity @var{Arity}. ".

compound_pure_type_term(Type, Term, Name, Arity):- 
   nonvar(Type),
   Type = ^(Term),
   nonvar(Term),
   functor(Term, Name, Arity),
   Arity > 0,
   % \+ Term = [_|_], 
   !.
compound_pure_type_term(Type, Type, (.), 2):- 
   nonvar(Type),
   Type = [_|_].


:- type rule_type_symbol(Type)

; "@var{Type} is a @tt{(possibly parametric) type symbol}  
   that should be defined by a (possibly parametric) type rule.".

rule_type_symbol(Type):-
   non_par_rule_type_symbol(Type)
 ; par_rule_type_symbol(Type).

:- pred non_par_rule_type_symbol(Type)

# "@var{Type} is a @tt{non-parametric type symbol} 
   that should be defined by a type rule.".

non_par_rule_type_symbol(Type):-
   atom(Type),
   non_par_pred_arity(Type/0).

non_par_pred_arity(Pred):-
   \+ Pred = term/0,
   \+ Pred = bot/0,
   \+ Pred = gnd/0,
   \+ Pred = num/0,
   \+ Pred = atm/0,
   \+ Pred = var/0,
   \+ Pred = []/0.

:- pred par_rule_type_symbol(Type)

# "@var{Type} is a @tt{parametric type symbol} that should be defined by a 
   parametric type rule.".

par_rule_type_symbol(Type):-
   nonvar(Type),
   functor(Type, F, A),
   A > 0,
   par_pred_arity(F/A).

par_pred_arity(Pred):-
   \+ Pred = (^)/1,
   \+ Pred = (.)/2,
   \+ Pred = regtype/1,
   \+ Pred = (=)/1.

% CREATION OF TYPES.

set_top_type(term).
set_ground_type(gnd).
set_bottom_type(bot).
set_numeric_type(num).
set_var_type(var).
set_atom_type(atm).
