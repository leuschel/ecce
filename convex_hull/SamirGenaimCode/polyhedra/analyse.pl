:- module(analyse,[go/1, flag/0]).
/* File: analyse.pl */

:- use_module(input,[load_file/1, my_clause/2]).
:- use_module(symbolic_norm,[term_size_exp/2]).
:- use_module(db,[ get_prev_fact/2,
                   cond_assert_size/1,
                   fact/1 ]).

:- use_module(polyhedra,[ projection/2,
	                         widen/3,
			         hull/3]).

:- use_module(library(lists),[append/3]).


%% interface
go(File) :-
      retractall(fact((_:-_))),
      load_file(File),
      iterate(1),
      showfacts.

%% control (count iterations so as to know when to widen)
:- dynamic flag/0.

iterate(I) :- operator(I), fail.
iterate(I) :- flag, retractall(flag), I1 is I+1,  iterate(I1).
iterate(_).

%% logic
operator(I) :-
    my_clause(Head,Body),
    prove(Body,[],Cs),
    projection((Head :- Cs),(ProjHead :- ProjCs)),
    ( % "is there a previous relevant fact already derived?"
     get_prev_fact( Head, (PrevHead :- PrevCs) ) -> 
        ( % "to widen? or just to lub?"
	  I mod 3 =:= 0 ->
	    lub((PrevHead :- PrevCs), (ProjHead :- ProjCs), (NewHead1 :- NewCs1)),
            widen((NewHead1 :- NewCs1), (PrevHead :- PrevCs), (NewHead :- NewCs))
        ;
	    lub((PrevHead :- PrevCs), (ProjHead :- ProjCs), (NewHead :- NewCs))
        )
    ; 
        NewHead=ProjHead, NewCs=ProjCs
    ),
    cond_assert_size(fact((NewHead :- NewCs))).



prove([B|Bs],ConstraintsIn,ConstraintsOut) :-
	fact((B :- ConstraintsB)),
	meet(ConstraintsIn,ConstraintsB,ConstraintsNew),
	prove(Bs,ConstraintsNew,ConstraintsOut).

prove([unify(A,B)|Bs],ConstraintsIn,ConstraintsOut) :-
	unify(A,B,ConstraintAB),
	meet(ConstraintsIn,ConstraintAB,ConstraintsNew),
	prove(Bs,ConstraintsNew,ConstraintsOut).

prove([],Constraints,Constraints).
 

%%%
%%% The MEET
%%%
meet(Constraints1,Constraints2,Meet) :-
	append(Constraints1,Constraints2,Meet).


%%%
%%% Abstract Unification
%%% (abstract the Term and take the new constraint)

unify(Var, Term, [Var = Expr]) :-
	term_size_exp(Term,Expr).

%%%
%%% The LUB of A and B is C
%%% (call the convex hull package)
 
lub(A,B,C) :- 
    hull(A,B,C).

showfacts :-
    fact(F),
    numbervars(F,0,_),
    (F = ':-'(H,C) -> my_portray_clause(user,H,C) ; (print(F),nl)),
    fail.
showfacts :- true.




% added by M. Leuschel:

my_portray_clause(Stream,H,C) :- copy_term((H,C),(CH,CC)),
   numbervars((CH,CC),0,_),
   write(Stream,CH), write(Stream,' :- '),nl(Stream),
   write(Stream,'      {'), my_body(CC,Stream),
   write(Stream,'}.'),nl(Stream).
   
 my_body([],S) :- !.
 my_body([Call|T],S) :- !,
      my_call(S,Call), (T=[] -> true ; write(S,', ')),
      my_body(T,S).
 my_body(C,S) :- write(S,C).

my_call(S,C) :- infix(C,X,Op,Y), !, my_arg(S,X), write(S,Op), my_arg(S,Y).
my_call(S,C) :- write(S,C).


my_arg(S,C) :- ainfix(C,X,Op,Y), !, my_arg(S,X), write(S,Op), my_arg(S,Y). 
         /* ok with priorities ?? probably yes because we have linear constraints only */
my_arg(S,rat(X,1)) :- !,write(S,X).
my_arg(S,rat(X,Y)) :- !, write(S,X), write(S,'/'), write(S,Y).
my_arg(S,A) :- write(S,A).


infix('='(A,B),A,'=',B).
infix('>'(A,B),A,'>',B).
infix('>='(A,B),A,'>=',B).
infix('<'(A,B),A,'<',B).
infix('=<'(A,B),A,'=<',B).


ainfix('*'(A,B),A,'*',B).
ainfix('+'(A,B),A,'+',B).
ainfix('-'(A,B),A,'-',B).


