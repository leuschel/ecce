:- module(native_props,
        [  covered/2
	 , linear/1
         , mshare/1
	 , clique/1
	 , clique_1/1
         , nonground/1
         , fails/1
         , not_fails/1
         , possibly_fails/1
         , covered/1
         , not_covered/1 
         , is_det/1
         , non_det/1
         , possibly_nondet/1
         , mut_exclusive/1
         , not_mut_exclusive/1
         , size_lb/2
         , size_ub/2
         , size/2
         , size_o/2
         , steps_lb/2 
         , steps_ub/2  
         , steps/2  
         , steps_o/2  
         , finite_solutions/1
         , terminates/1
	 , constraint/1
        ],
        [assertions]).

:- reexport(library('andprolog/andprolog_rt'),[indep/1,indep/2]).
:- comment(doinclude,indep/1).
:- comment(doinclude,indep/2).

%% :- reexport(engine(term_typing),[ground/1,nonvar/1,var/1]).
%% :- comment(doinclude,ground/1).
%% :- comment(doinclude,nonvar/1).
%% :- comment(doinclude,var/1).
%% 
%% :- reexport(engine(basic_props),[regtype/1, native/2, native/1, sideff/2,
%%         term/1, int/1, nnegint/1, flt/1, num/1, atm/1, struct/1, gnd/1]).

:- reexport(library(terms_check),[instance/2]).
:- comment(doinclude,[instance/2]).

:- use_module(library(terms_vars),[varsbag/3]).
:- use_module(library(sort),[sort/2]).
:- use_module(library(lists)).

% --------------------------------------------------------------------------
:- comment(title,"Properties which are native to analyzers").

:- comment(author,"Francisco Bueno").
:- comment(author,"Manuel Hermenegildo").
:- comment(author,"Pedro Lopez").

:- comment(module,"@cindex{properties, native} This library contains a
   set of properties which are natively understood by the different program
   analyzers of @apl{ciaopp}.  They are used by @apl{ciaopp} on output
   and they can also be used as properties in assertions.").

:- comment(usage,"@tt{:- use_module(library('assertions/native_props'))}

   or also as a package @tt{:- use_package(nativeprops)}.

   Note the different names of the library and the package.").

% --------------------------------------------------------------------------

:- comment(constraint(C)," @var{C} contains a list of linear (in)equalities
   that relate variables and @tt{int} values. For example,  @tt{[A < B + 4]} 
   is a constraint while @tt{[A < BC + 4]} or @tt{[A = 3.4, B >= C]} are not.").
:- true prop constraint(C) + native
# "@var{C} is a list of linear equations".
constraint([]).
constraint([Cons|Rest]):-
	constraint_(Cons),
	constraint(Rest).

constraint_(=(Lin_Expr1,Lin_Expr2)):-
	lin_expr(Lin_Expr1),
	lin_expr(Lin_Expr2).
constraint_(=<(Lin_Expr1,Lin_Expr2)):-
	lin_expr(Lin_Expr1),
	lin_expr(Lin_Expr2).
constraint_(>=(Lin_Expr1,Lin_Expr2)):-
	lin_expr(Lin_Expr1),
	lin_expr(Lin_Expr2).
constraint_(<(Lin_Expr1,Lin_Expr2)):-
	lin_expr(Lin_Expr1),
	lin_expr(Lin_Expr2).
constraint_(>(Lin_Expr1,Lin_Expr2)):-
	lin_expr(Lin_Expr1),
	lin_expr(Lin_Expr2).

:- push_prolog_flag(multi_arity_warnings,off).
:- push_prolog_flag(discontiguous_warnings,off).

lin_expr(PPL_Var):-
	ppl_var(PPL_Var),!.
lin_expr(Coeff):-
	coefficient(Coeff).
lin_expr(+(Lin_Expr),Vars,+(New_Lin_Expr)):-
	lin_expr(Lin_Expr,Vars,New_Lin_Expr).
lin_expr(-(Lin_Expr)):-
	lin_expr(Lin_Expr).
lin_expr(+(Lin_Expr1,Lin_Expr2)):-
	lin_expr(Lin_Expr1),
	lin_expr(Lin_Expr2).
lin_expr(-(Lin_Expr1,Lin_Expr2)):-
	lin_expr(Lin_Expr1),
	lin_expr(Lin_Expr2).
lin_expr(*(Coeff,Lin_Expr)):-
	coefficient(Coeff),
	lin_expr(Lin_Expr).
lin_expr(*(Lin_Expr,Coeff)):-
	coefficient(Coeff),
	lin_expr(Lin_Expr).

:- pop_prolog_flag(discontiguous_warnings).
:- pop_prolog_flag(multi_arity_warnings).

ppl_var(Var):-
	var(Var).
coefficient(Coeff):-
	ground(Coeff),
	int(Coeff).


:- comment(covered(X,Y), "All variables occuring in @var{X} occur also
   in @var{Y}.").

:- true prop covered(X,Y) + native # "@var{X} is covered by @var{Y}.".

covered(X,Y):-
	varsbag(X,VarsX,[]),
	varsbag(Y,VarsY,[]),
	sublist(VarsX,VarsY).

:- comment(linear(X), "@var{X} is bound to a term which is linear,
   i.e., if it contains any variables, such variables appear only once
   in the term. For example, @tt{[1,2,3]} and @tt{f(A,B)} are linear
   terms, while @tt{f(A,A)} is not.").

:- true prop linear(X) + native
# "@var{X} is instantiated to a linear term.".

linear(T):-
	varsbag(T,VarsBag,[]),
	sort(VarsBag,VarsSet),
	length(VarsBag,N),
	length(VarsSet,N).

:- comment(mshare(X), "@var{X} contains all @index{sharing sets}
   @cite{jacobs88,abs-int-naclp89} which specify the possible variable
   occurrences in the terms to which the variables involved in the
   clause may be bound. Sharing sets are a compact way of representing
   groundness of variables and dependencies between variables. This
   representation is however generally difficult to read for
   humans. For this reason, this information is often translated to
   @prop{ground/1}, @prop{indep/1} and @prop{indep/2} properties,
   which are easier to read.").

:- prop mshare(X) + native(sharing(X))
# "The sharing pattern is @tt{@var{X}}.".

:- impl_defined(mshare/1).

:- comment(clique(X), "@var{X} is a set of variables of interest, much the
   same as a sharing group but @var{X} represents all the sharing groups in
   the powerset of that variables. Similar to a sharing group, a clique is
   often translated to @prop{ground/1}, @prop{indep/1} and @prop{indep/2}
   properties.").

:- prop clique(X) + native(clique(X))
# "The clique pattern is @tt{@var{X}}.".

:- impl_defined(clique/1).

:- comment(clique_1(X), "@var{X} is a set of variables of interest, much
   the same as a sharing group but @var{X} represents all the sharing
   groups in the powerset of that variables but disregarding the
   singletons. Similar to a sharing group, a clique_1 is often translated
   to @prop{ground/1}, @prop{indep/1} and @prop{indep/2} properties.").

:- prop clique_1(X) + native(clique_1(X))
# "The 1-clique pattern is @tt{@var{X}}.".

:- impl_defined(clique_1/1).



:- prop nonground(X) + native(not_ground(X))
# "@tt{@var{X}} is not ground.".

:- impl_defined(nonground/1).


%%%%%%%%%%%%%%%%%%%%%% FAILS
%
:- comment(fails(X), "Calls of the form @var{X} fail.").
%
:- true prop fails(X) + native # "Calls of the form @var{X} fail.".
%
:- meta_predicate fails( goal ).
%
fails( X ) :-
	if( X , throw( rtcheck( fails , fail , X  ) ) , true ).


%%%%%%%%%%%%%%%%%%%%%% NOT_FAILS
%
:- comment(not_fails(X), "Calls of the form @var{X} produce at least
   one solution, or not terminate @cite{non-failure-iclp97}.").
%
:- true prop not_fails(X) + native # 
	"All the calls of the form @var{X} do not fail.".
%
:- meta_predicate not_fails( goal ).
%
not_fails( X ) :-
	if( X , true , throw( rtcheck( nf , fail , X  ) ) ).



:- comment(possibly_fails(X), "Non-failure is not ensured for any call
of the form @var{X} @cite{non-failure-iclp97}. In other words, nothing
can be ensured about non-failure nor termination of such calls.").

:- prop possibly_fails(X)
# "Non-failure is not ensured for calls of the form @var{X}.".

:- meta_predicate possibly_fails( goal ).

possibly_fails( X ) :-
	call( X ).


:- comment(covered(X), "For any call of the form @var{X} there is at
least one clause whose test succeeds (i.e. all the calls of the form
@var{X} are covered.) @cite{non-failure-iclp97}.").

:- prop covered(X) 
# "All the calls of the form @var{X} are covered.".

:- impl_defined(covered/1).

:- comment(not_covered(X), "There is some call of the form @var{X} for
which there is not any clause whose test succeeds
@cite{non-failure-iclp97}.").

:- prop not_covered(X) 
# "Not all of the calls of the form @var{X} are covered.".

:- impl_defined(not_covered/1).

:- comment(is_det(X), "All calls of the form @var{X} are
deterministic, i.e. produce at most one solution, or not terminate.").



:- prop is_det(X)
# "All calls of the form @var{X} are deterministic.".

:- meta_predicate is_det( goal ).

:- data '$is_det'/2.

is_det( X ) :- 
	( '$is_det'(CNT,_)-> true;CNT=0),
	CNT1 is CNT + 1,
	( asserta_fact( '$is_det'(CNT1,0) )
	; retractall_fact( '$is_det'(CNT1,_) ),
	  fail),
	call( X ),
	retract_fact( '$is_det'(CNT1,C) ),
	(
	    C = 0
	->
	    asserta_fact( '$is_det'(CNT1,1) )
	;
	    throw( rtcheck( det , nodet , X  ) )
	).
is_det( _ ).


:- comment(non_det(X), "All calls of the form @var{X} are not
deterministic, i.e., produce several solutions.").



:- prop non_det(X)
# "All calls of the form @var{X} are not deterministic.".

:- impl_defined(non_det/1).

:- comment(possibly_nondet(X), "Non-determinism is not ensured for all
calls of the form @var{X}. In other words, nothing can be ensured
about determinacy nor termination of such calls.").


:- prop possibly_nondet(X)
# "Non-determinism is not ensured for calls of the form @var{X}.".

:- meta_predicate possible_nondet( goal ).

possibly_nondet( X ) :-
	call( X ).


%% disjoint(X)
%% # "Calls of the form @var{X} select at most one clause.".

:- comment(mut_exclusive(X), "For any call of the form @var{X} at most one
clause succeeds, i.e. clauses are pairwise exclusive.").

:- prop mut_exclusive(X)
# "For any call of the form @var{X} at most one clause succeeds.".

:- impl_defined(mut_exclusive/1).

:- comment(not_mut_exclusive(X), "Not for all calls of the form @var{X} at
most one clause succeeds. I.e. clauses are not disjoint for some
call.").

 %% For any call of the form @var{X} at most one
 %% clause succeeds, i.e. clauses are pairwise exclusive.").

:- prop not_mut_exclusive(X)
# "Not for all calls of the form @var{X} at most one clause
  succeeds.".

:- impl_defined(not_mut_exclusive/1).

:- comment(size_lb(X, Y), "The minimum size of the terms to which
the argument @var{Y} is bound to is given by the expression
@var{Y}. Various measures can be used to determine the size of an
argument, e.g., list-length, term-size, term-depth, integer-value,
etc. @cite{caslog}.").

:- prop size_lb(X,Y)
# "@var{Y} is a lower bound on the size of argument @var{X}.".

:- impl_defined(size_lb/2).

:- comment(size_ub(X, Y), "The maximum size of the terms to which
the argument @var{Y} is bound to is given by the expression
@var{Y}. Various measures can be used to determine the size of an
argument, e.g., list-length, term-size, term-depth, integer-value,
etc. @cite{caslog}.").

:- prop size_ub(X,Y)
# "@var{Y} is a upper bound on the size of argument @var{X}.".

:- impl_defined(size_ub/2).

%% upper_size(X,Y)
%% # "The maximum size of arguments of calls of the form @var{X} are
%%    given by the expression @var{Y}.".

:- prop size(X,Y)
# "@var{Y} is the size of argument @var{X}.".

:- impl_defined(size/2).

:- prop size_o(X,Y)
# "The size of argument @var{X} is in the order of @var{Y}.".

:- impl_defined(size_o/2).

% ----------------------------------------------------------------------------
:- comment(steps_lb(X, Y), "The minimum computation time (in
resolution steps) spent by any call of the form @var{X} is given by
the expression @var{Y} @cite{low-bounds-ilps97,granularity-jsc}").

:- prop steps_lb(X,Y) 
# "@var{Y} is a lower bound on the cost of any call of the form
@var{X}.".

:- impl_defined(steps_lb/2).

%% lower_time(X,Y)
%% # "The minimum computation time spent by calls of the form @var{X} is
%%    given by the expression @var{Y}.".

:- comment(steps_ub(X, Y), "The maximum computation time (in
resolution steps) spent by any call of the form @var{X} is given by
the expression @var{Y} @cite{caslog,granularity-jsc}").

:- prop steps_ub(X,Y) 
# "@var{Y} is a upper bound on the cost of any call of the form
@var{X}.".

:- impl_defined(steps_ub/2).

%% upper_time(X,Y)
%% # "The maximum computation time spent by calls of the form @var{X} is
%%    given by the expression @var{Y}.".

:- comment(steps(X, Y), "The time (in resolution steps) spent by any
call of the form @var{X} is given by the expression @var{Y}").

:- prop steps(X,Y) 
# "@var{Y} is the cost (number of resolution steps) of any call of the form
@var{X}.".

:- impl_defined(steps/2).

:- prop steps_o(X,Y) 
# "@var{Y} is the complexity order of the cost of any call of the form
@var{X}.".

:- impl_defined(steps_o/2).

% Added By EMM
%----------------------------------------------------------------------------

:- comment(resource_lb(X, Y), "The minimum resource spent by any call
   of the form @var{X} is given by the expression @var{Y}").

:- prop resource_lb(X,Y) 
# "@var{Y} is a lower bound on the cost of any call of the form
@var{X}.".

:- impl_defined(resource_lb/2).

:- comment(resource_ub(X, Y), "The maximum resource spent by any call
   of the form @var{X} is given by the expression @var{Y}").

:- prop resource_ub(X,Y) # "@var{Y} is a upper bound on the resources
   of any call of the form @var{X}.".

:- impl_defined(resource_ub/2).

:- comment(arith(X, Y), "The time (in resolution arith) spent by any
call of the form @var{X} is given by the expression @var{Y}").

:- prop resource(X,Y) # "@var{Y} is the resources spent of any call of
   the form @var{X}.".

:- impl_defined(resource/2).

:- prop resource_o(X,Y) # "@var{Y} is the complexity order of the
   resources spent by any call of the form @var{X}.".

:- impl_defined(resource_o/2).

% ----------------------------------------------------------------------------
% End Added By EMM

:- prop sideff_pure(X) 
# "@var{X} is pure, i.e., has no side-effects.".

:- impl_defined(sideff_pure/1).

:- prop sideff_soft(X) 
# "@var{X} has @index{soft side-effects}, i.e., those not affecting
   program execution (e.g., input/output).".

:- impl_defined(sideff_soft/1).

:- prop sideff_hard(X) 
# "@var{X} has @index{hard side-effects}, i.e., those that might affect
   program execution (e.g., assert/retract).".

:- impl_defined(sideff_hard/1).

:- comment(finite_solutions(X), "Calls of the form @var{X} produce a
   finite number of solutions @cite{non-failure-iclp97}.").

:- prop finite_solutions(X) # 
	"All the calls of the form @var{X} have a finite number of
         solutions.".

:- impl_defined(finite_solutions/1).

:- comment(terminates(X), "Calls of the form @var{X} always
   terminate @cite{non-failure-iclp97}.").

:- prop terminates(X) # 
	"All the calls of the form @var{X} terminate.".

:- impl_defined(terminates/1).

% Built-in in CiaoPP
:- prop entry_point_name/2.
% if you change this declaration, you have to change ciaoPP:
:- meta_predicate entry_point_name(goal,?).
:- impl_defined(entry_point_name/2).
:- comment(hide,entry_point_name/2).
