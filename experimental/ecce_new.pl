/* Ecce New Foundations */

:- use_module(prolog_reader).

:- dynamic conjunction/2.

conjunction(1,[app(X,Y,Z),app(Z,V,[1,2,3]),app(VV,WW,[1])]).


:- dynamic trans/3.
%trans(1,split([1]),2).
%trans(1,split([2]),3).

:- dynamic last_conjunction_id/1.
last_conjunction_id(1).



add_conjunction(Conj,ID) :- conjunction(ID,Conj),!. % to do: use hashing on skeleton
add_conjunction(Conj,ID) :- retract(last_conjunction_id(LID)),
  ID1 is LID+1, assert(last_conjunction_id(ID1)),
  ID=ID1,
  assert(conjunction(ID,Conj)).

open_conjunction(ID,Conj) :- conjunction(ID,Conj), Conj \=[], Conj\=fail, \+ trans(ID,_,_).

pr :- pr(1,1).

pr(ID,Indent) :- conjunction(ID,Conj),
      indent(Indent),print(ID), print(' : '), print(Conj),nl,
      trans(ID,Trans,NewID),
      indent(Indent), print(' --> '), print(Trans),
      (NewID>ID
        -> ID1 is ID+1, nl, pr(NewID,ID1)
        ;  print(' --> '), print(NewID),nl
      ),fail.
pr(ID,Indent).

indent(0).
indent(N) :- N>0, print(' + '), N1 is N-1, indent(N1).

% MAIN

run :- load_file('test.pl'),pe.

pe :- open_conjunction(ID,Conj),!,
      print(treating(ID,Conj)),nl,
      (try_split_conjunction(ID,Conj)
        -> true
        ;  print(unfolding(ID)),nl,
           unfold_conjunction(Conj,ID)
     ),pe.
 pe :- print(finished),nl,pr.


% BUP

:- dynamic bup_answer/2.
%bup_step :- conjunction(ID,Conj),
%            findall(X, trans(ID,unfold(..


% SPLITTING CONJUNCTIONS

try_split_conjunction(ID,Conjunction) :-
   set_up_indices(Conjunction,1,Indices),
   split_up_conjunction(Conjunction,ID,Indices).
   
:- use_module(library(terms),[term_variables/2]).
split_up_conjunction([],_,[]).
split_up_conjunction([Literal|T],ID,[Index1|TI]) :-
    term_variables(Literal,Vars),
    get_literals_with_joint_vars(T,TI,Vars,ShT,ShIndx,NShT,NShIndx),
    
    (Index1=1 -> (NShT = [] -> print(no_split),fail ; true) ; true), % otherwise no real split
    
    add_conjunction([Literal|ShT],NewID),
    assert(trans(ID,split([Index1|ShIndx]),NewID)),
    split_up_conjunction(NShT,ID,NShIndx).
    
get_literals_with_joint_vars([],[],_,[],[],[],[]).
get_literals_with_joint_vars([Literal|T],[Index|TI],Vars,ShLits,ShIndexes,NShLits,NShIndexes) :-
   term_variables(Literal,LitVars),
   (\+ disjoint(LitVars,Vars)
    ->  ShLits = [Literal|LT], ShIndexes = [Index|IT],
        NShLits = NLT, NShIndexes = NIT
    ;   ShLits = LT, ShIndexes = IT,
        NShLits = [Literal|NLT], NShIndexes = [Index|NIT]
   ),
   get_literals_with_joint_vars(T,TI,Vars,LT,IT,NLT,NIT).
  
   
disjoint(Vars1,Vars2) :- numbervars(Vars2,1,_),
   free_vars(Vars1).
free_vars([]).
free_vars([H|T]) :- var(H), free_vars(T).

set_up_indices([],_,[]).
set_up_indices([_|T],N,[N|NT]) :- N1 is N+1, set_up_indices(T,N1,NT).

% UNFOLDING CONJUNCTIONS

unfold_conjunction(Conj,ID) :- select_literal(Conj,Lit,Rest1,Rest2),
   get_clause_as_list(Lit,Body,ClauseRef),
   append(Body,Rest2,BR), append(Rest1,BR,Result),
    add_conjunction(Result,NewID),
    assert(trans(ID,unfold(ClauseRef),NewID)),
    fail.
unfold_conjunction(_,ID) :-
  (trans(ID,unfold(_),_) -> true ; add_conjunction(fail,NewID), assert(trans(ID,unfold(fail),NewID))).
 

select_literal([Lit|T],Lit,[],T). % leftmost
