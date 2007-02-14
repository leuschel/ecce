/* to use: ['~/CVS/ecce/ecce_source/bottomUp/adhoc_bup']. */

:- use_module([bottomUp]).
:- use_module(library(terms)).
:- use_module(library(lists)).

/* test query:
Leaf = [trans(_256130,_256131,_256132),
               ptrace(interleave(agent(z2),_256132),_256126),
rul__constraint__declaration([any(_256132),t244(_256130),any(_256131)],
 [proc(t224/1,[(t224(z2):-true)]),
  proc(t226/1,[(t226(agent(_256071)):-t224(_256071))]),
  proc(t244/1,[(t244(interleave(_256051,_256052)):-t226(_256051),
              t244(_256052)),(t244(agent(_256035)):-t224(_256035))]),
  proc(any/1,[(any(_256018):-true)])])
 ],
ad_hoc_bup_propagation(Leaf,NewLeaf)


Another test query:
ad_hoc_bup_propagation([trans(_858,_388,_855),
  trace(interleave(interleave(interleave(_857,_855),_377),stop),_383),
  rul__constraint__declaration([t92(_383),t540(interleave(_857,_855)),t62(_377),
   t75(interleave(_857,_858)),t37(_388)],
   [proc(t181/1,[(t181(v2):-true)]),
    proc(t54/1,[(t54(stop):-true)]),proc(t25/1,[(t25(b):-true)]),
    proc(t543/1,[(t543(interleave(_288,_289)):-t451(_288),t543(_289)),
    (t543(stop):-true),(t543(prefix(_264,_265)):-t25(_264),t54(_265))]),
    proc(t451/1,[(t451(agent(_240)):-t181(_240)),
     (t451(interleave(_228,_229)):-t451(_228),t543(_229)),
     (t451(stop):-true)]),proc(t37/1,[(t37(b):-true),(t37(a):-true)]),
     proc(t75/1,[(t75(agent(_175)):-t181(_175)),(t75(interleave(_163,_164)):-t75(_163),t62(_164))]),
     proc(t62/1,[(t62(stop):-true),(t62(prefix(_131,_132)):-t25(_131),t54(_132))]),
     proc(t540/1,[(t540(interleave(_106,_107)):-t451(_106),t543(_107)),(t540(stop):-true)]),
     proc(t92/1,[(t92([]):-true),
 (t92([_67|_68]):-t37(_67),t92(_68))])])],R)
*/

/* adapts the add_leaves procedure from ecce_main
  to perform an ad-hoc bottom-up-answer propagation */

add_leaves(GoalID,Goal,Chtree) :-
	get_leaf(Chtree,Goal,Leaf,ChPosition),
	print(calling(ad_hoc_bup_propagation(Leaf,NewLeaf))),nl,
	ad_hoc_bup_propagation(Leaf,NewLeaf),
	((NewLeaf = [])
	 -> (print('### add_leaves: Leaf = []'),nl)
	 ;  (pp_mnf(add_gt_leaf(GoalID,NewLeaf,ChPosition,_LeafID)),
	     print(added_leaf(GoalID,_LeafID,NewLeaf)),nl
	    )
	),
	fail.
add_leaves(_,_,_).


ad_hoc_bup_propagation(Goal,NewGoal) :-
 divide_constraint_rul_goal(Goal,OGoal,RCD),
 RCD = rul__constraint__declaration(CGoal,RUL),
 /* Atom=trans(_,_,_),
    member(Atom,OGoal), */
 \+(ground(OGoal)),
 print('goal before: '), print(OGoal),nl,
 generate_new_rul_constraints(OGoal,CGoal,RUL,NewRULConstrDecl),
 print('goal: '),print(OGoal),nl,
 print_rul(NewRULConstrDecl),
 append(OGoal,[NewRULConstrDecl],NewGoal),!.
ad_hoc_bup_propagation(Goal,Goal).

generate_new_rul_constraints(OGoal,CGoal,RUL,NewRULConstrDecl) :-
 default_input_file(InFile),
 tell(InFile),
 print_original_program,
 print_rul_prog(RUL),
 term_variables(OGoal,VList),
 MyQueryAtom =.. [my_query_atom|VList],
 VList = [_|_], /* at least one variable */
 nl,nl,
 print(MyQueryAtom),
 print(' :- '),nl,
 print('  '),
 print_list(CGoal), print(','),nl,
 print('  '),print_list(OGoal),
 print('.'),nl,
 told,
 get_bup_rulconstraints(MyQueryAtom,NewRULConstrDecl).
 
print_list([]) :- print(true).
print_list([H]) :- print(H).
print_list([H,H2|T]) :- print(H),print(', '), print_list([H2|T]).
 

% to test: ?- get_bup_rulconstraints(test(X,Y),'test.pl',RUL), print_rul(RUL).

print_rul(rul__constraint__declaration(Goal, Prog)) :- !,
   print('RUL Constraints: '),
   print(Goal),
   nl,
   print('RUL Program:'),
   nl,
   print_rul_prog(Prog).


print_rul_prog([]).
print_rul_prog([Proc|T]) :-
	print_rul_proc(Proc),
	print_rul_prog(T).

print_rul_proc(proc(_,Defs)) :- nl,
	print_rul_defs(Defs).
  
print_rul_defs([]).
print_rul_defs([Def|T]) :-
    print('  '),
	print(Def),
	print('.'),
	nl,
	print_rul_defs(T).
	
	
print_original_program	:- list_database_wo_numbers.

list_database_wo_numbers :-
	claus(Nr,Head,Body),
	print_clause_with_nl(Head,Body),fail.
list_database_wo_numbers :- nl.

/* print_original_program :-
  print('trans(prefix(A,X),A,X).
trans(par(X,Y),A,par(X2,Y2)) :- trans(X,A,X2), trans(Y,A,Y2).
trans(interleave(X,Y),A,interleave(X2,Y)) :- trans(X,A,X2).
trans(interleave(X,Y),A,interleave(X,Y2)) :- trans(Y,A,Y2).
trans(choice(X,_Y),A,X2) :- trans(X,A,X2).
trans(choice(_X,Y),A,Y2) :- trans(Y,A,Y2).
trans(agent(X),A,Y) :- agent_def(X,Def), trans(Def,A,Y).


agent_def(u2,prefix(a,interleave(prefix(b,stop),agent(u2)))).
agent_def(v2,prefix(a,interleave(agent(v2),prefix(b,stop)))).
agent_def(w2,choice(prefix(b,stop),prefix(a,prefix(b,agent(w2))))).
agent_def(z2,prefix(a,interleave(agent(z2),agent(z2)))).

ptrace(_X,[]).
ptrace(X,[A|T]) :- trans(X,A,Y), ptrace(Y,T).

test1(T) :- ptrace(agent(z2),T).
test2(T) :- trace(agent(v2),T).
test3(T) :- trace(agent(w2),T).
test4(T) :- trace(agent(w2),T), trace(agent(v2),T).

stop(stop).
stop(par(X,Y)) :- stop(X),stop(Y).
stop(interleave(X,_Y)) :- stop(X).
stop(interleave(_X,Y)) :- stop(Y).
stop(choice(X,_Y)) :- stop(X).
stop(choice(_X,Y)) :- stop(Y).

trace(X,[]) :- stop(X).
trace(X,[A|T]) :- trans(X,A,Y), trace(Y,T).'),
 nl.
*/

