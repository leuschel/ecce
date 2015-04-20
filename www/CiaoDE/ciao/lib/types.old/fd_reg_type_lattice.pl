
% DEFINITION OF SIMPLE TYPES

 % DEFINITION OF THE SYMBOLS USED IN THE LATTICE

:- pred native_type_symbol(X)
   # "@var{X} is a constant defining a @tt{native type} of the lattice
      (bottom point excluded).".

native_type_symbol(T):- top_type(T).
native_type_symbol(T):- ground_type(T).
native_type_symbol(T):- float_type(T).
native_type_symbol(T):- numeric_type(T).
native_type_symbol(T):- atom_type(T).
native_type_symbol(T):- var_type(T).
native_type_symbol(T):- struct_type(T).
native_type_symbol(T):- rat_type(T).
native_type_symbol(T):- int_type(T).
native_type_symbol(T):- nnegint_type(T).
native_type_symbol(T):- anyfd_type(T).

top_type(X):- X == term.
bot_type(X):- X == bot.
ground_type(X):- X == gnd.
float_type(X):- X == flt.
numeric_type(X):- X == num.
atom_type(X):- X == atm.
var_type(X):- X == var.
struct_type(X):- X == struct.
% fdtypes
rat_type(Type):- Type == rat. 
int_type(Type):- Type == int.
nnegint_type(Type):- Type == nnegint. 
anyfd_type(Type):- Type == anyfd.
 
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

# "@var{Type} is a @tt{(possibly parametric) type symbol}  
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
  fdtypes_non_par_pred_arity(Pred).

fdtypes_non_par_pred_arity(Pred):-
   % fdtypes
   \+ Pred = int/0,
   \+ Pred = rat/0,
   \+ Pred = nnegint/0,
   \+ Pred = anyfd/0,
   % regtypes
   \+ Pred = term/0,
   \+ Pred = bot/0,
   \+ Pred = gnd/0,
   \+ Pred = num/0,
   \+ Pred = flt/0,
   \+ Pred = atm/0,
   \+ Pred = var/0,
   \+ Pred = struct/0,
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

% Used in type operations

:- pred base_type_symbol(X)

# "@var{X} is a @tt{base type symbol}. Different base type symbols must
define disjoint sets of terms.".

base_type_symbol(X):- 
  base_type_symbol_fdtypes(X).            

base_type_symbol_fdtypes(X):-
  int_type(X);
  float_type(X);
  nnegint_type(X);
  atom_type(X);
  var_type(X);
  anyfd_type(X);
  rat_type(X);
  numeric_type(X).
%% ; struct_type(X). %% Comented out by PLG


% CONSTANTS

atom_constant(Type, Type):-
   nonvar(Type), 
   Type = [], 
   !.
atom_constant(Type, Term):- 
   nonvar(Type),
   Type = ^(Term),
   atom(Term).

number_constant(Type, Type):- 
   number(Type),
   !.
number_constant(Type, Term):- 
   nonvar(Type),
   Type = ^(Term),
   number(Term).

float_constant(Type, Type):- 
   float(Type),
   !.
float_constant(Type, Term):- 
   nonvar(Type),
   Type = ^(Term),
   float(Term).

% fdtypes

int_constant(Type, Type):- 
   integer(Type),
   !.
int_constant(Type, Term):- 
   nonvar(Type),
   Type = ^(Term),
   integer(Term).

rat_constant(Type, Type):- 
   number(Type),
   !.
rat_constant(Type, Term):- 
   nonvar(Type),
   Type = ^(Term),
   number(Term).

nnegint_constant(Type, Type):- 
   integer(Type), 
   Type > 0,
   !.
nnegint_constant(Type, Term):- 
   nonvar(Type),
   Type = ^(Term),
   integer(Term), Term > 0.

% 
:- pred constant_symbol(Type, Constant)

# "@var{Type} is the type representation of the @tt{constant} @var{Constant}
   (numeric or non-numeric).".

constant_symbol(Type, Constant):- 
   atom_constant(Type, Constant);
   number_constant(Type, Constant); 
   int_constant(Type, Constant) ;
   float_constant(Type, Constant) ;
   rat_constant(Type, Constant) ;
   nnegint_constant(Type, Constant).

  
% CREATION OF TYPES.

set_top_type(term).
set_ground_type(gnd).
set_bottom_type(bot).
set_numeric_type(num).
set_var_type(var).
set_atom_type(atm).
% more:
set_float_type(flt).
set_struct_type(struct).
% fdtypes:
set_rat_type(rat). 
set_int_type(int).
set_nnegint_type(nnegint). 
set_anyfd_type(anyfd).

% This gives the "analyzable" pred definitions of the basic types
% (whose intended definitions are in basictypes)
% Does not work!!!! (because of magic transformation)
basic_types_pred_defs(fdtypes,[]).
%% basic_types_pred_defs(fdtypes,
%% 	[ (clause(term(_),true),clid),
%% 	  (clause(gnd(X),ground(X)),clid),
%% 	  (clause(num(X),number(X)),clid),
%% 	  (clause(atm(X),atom(X)),clid),
%% 	  (clause(flt(X),float(X)),clid),
%% 	  (clause(struct(X),compound(X)),clid),
%% 	%??  (clause(var(X),var(X)),clid),
%% 	% These four do belong in fdtypes
%% 	% (clause(rat(X),??),clid),
%% 	% (clause(int(X),??),clid),
%% 	% (clause(nnegint(X),??),clid),
%% 	% (clause(anyfd(X),??),clid),
%% 	  (clause(regtype(_,_),true),clid)
%% 	]).
basic_types_pred_defs(regtypes,
	[ (clause(term(_),true),clid),
	  (clause(gnd(X),ground(X)),clid),
	  (clause(num(X),number(X)),clid),
	  (clause(atm(X),atom(X)),clid),
	  (clause(flt(X),float(X)),clid),
%	  (clause(struct(X),compound(X)),clid),  compund not a builtin
	  (clause(rat(X/Y),(integer(X),integer(Y))),clid),
	  (clause(int(X),integer(X)),clid),
	  (clause(nnegint(X),(integer(X),X>=0)),clid),
	  (clause(anyfd(_),true),clid),
	  (clause(regtype(_,_),true),clid),
	% A VERY UGLY HACK, I KNOW (PBC)
	  (clause('SYSCALL'(_),true),clid)
	]).
