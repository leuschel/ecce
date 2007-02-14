%###################################################################
%######### MODULE UNFOLD: Performing Anti-GoalRULification ######### 
%###################################################################

:- module(unfold2, [l_goalUnfolding/2]). /* Michael, 24.10.2001 */

/* This module is the successor of the old module "unfold.pl" which
   had provided the procedure l_goalUnfolding/5. In that procedure,
   the variables of the input goal(s) had been replaced explicitly
   by somewhat inefficient means of term-rewriting. The new method
   l_goalUnfolding/2 efficiently exploits built-in unification for
   replacing the goal variables by terms. Moreover, the new method
   can deal with SICSTUS built-in goals (like "\="), which will be
   used as meta-constraints in order to improve the unfolding re-
   sult in non-deterministic cases. For example, the non-determi-
   nistic definition {(p(f(X)):-q(X)),(p(a):-true)} becomes deter-
   ministic as soon as we are able to specify Y\=a as a meta-con-
   straint for p(Y).
*/
:- use_module(library(lists)).
:- use_module(library(terms)).

:- use_module(prePostCon).
:- use_module(self_check_rul).

%###################################################################


l_goalUnfolding(G,R) :-
   pp_mnf(unfold2:l_goalUnfolding2(G,R)).

:- initialization(assert_pre(unfold2:l_goalUnfolding2(G,R),
    (prePostCon:isRCD(R)))).
:- initialization(assert_post(unfold2:l_goalUnfolding2(G,R),true)).

l_goalUnfolding2(Goal,RULCD) :-
  extract_built_ins(Goal,BIns),!,
  RULCD = rul__constraint__declaration(RC,RULP),
  unfold_constraint(RC,BIns,RULP).

%-------------------------------------------------

extract_built_ins([],[])  :- !.

extract_built_ins([H|T],Result) :- !,
   (user:is_built_in_literal(H)
    -> (Result = [H|R2])
    ;  (Result = R2)
   ),
   extract_built_ins(T,R2).

extract_built_ins(_X,[]).

%--------------------------------------------------

built_ins_fail(ListOfBuiltIns) :-
  user:dead_built_in(ListOfBuiltIns,_AnyAtom,_AnySelLiteral).

%--------------------------------------------------

determinate_rul_constraint(T,BIns,RUL,SingleSolution) :-
   T =.. [TypeFunctor,_Arg],
   findall((T,Body),
     (member(proc('/'(TypeFunctor,1), TFDef),RUL),
      member((T :- Body), TFDef),
      \+(built_ins_fail(BIns))),
     [(T,SingleSolution)]).

%--------------------------------------------------

unfold_constraint([],_,_).

unfold_constraint([C|T],BIns,RUL) :-
   (determinate_rul_constraint(C,BIns,RUL,Body)
    -> (mnf(unfold2:convert_comma_to_list(Body,BList)),
        unfold_constraint(BList,BIns,RUL)
       )
    ;  true),
   unfold_constraint(T,BIns,RUL).

%--------------------------------------------------

convert_comma_to_list((A,B),[A|CB]) :- !,
  convert_comma_to_list(B,CB).

convert_comma_to_list(X,[X]).

%--------------------------------------------------

extract_built_ins([],[]).

extract_built_ins([H|T],Result) :-
   (user:is_built_in_literal(H)
    -> (Result = [H|R2])
    ;  (Result = R2)
   ),
   extract_built_ins(T,R2).

%==================================================
%##################################################
%==================================================
