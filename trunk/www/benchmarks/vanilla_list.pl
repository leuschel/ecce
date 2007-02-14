% vanilla_list.pl
%
% This is a plain vanilla interpreter for simple Prolog programs
% As object programs we have provided append as well as a 
% representation of another copy of the vanilla interpreter which
% in turn has a (representation of a) representation of append.

solve([]).
solve([A|T]) :- solve_atom(A), solve(T).

solve_atom(A) :- my_clause(A,B), solve(B).


my_clause(app([],L,L),[]).
my_clause(app([H|X],Y,[H|Z]),[app(X,Y,Z)]).
my_clause(p,[p]).
my_clause(solve2([]),[]).
my_clause(solve2([A|T]), [solve_atom2(A), solve2(T)]).
my_clause(solve_atom2(A), [my_clause2(A,B), solve2(B)]).
my_clause(my_clause2(app([],L,L),[]),[]).
my_clause(my_clause2(app([H|X],Y,[H|Z]),[app(X,Y,Z)]),[]).


test(R) :-
   solve_atom(solve_atom2(app([a,b,c],[d,e,f,g],R))).
app1(X,Y,R) :- /* equivalent to append */
   solve_atom(app(X,Y,R)). 
app2(X,Y,R) :-  /* also equivalent to append */
   solve_atom(solve_atom2(app(X,Y,R))).
