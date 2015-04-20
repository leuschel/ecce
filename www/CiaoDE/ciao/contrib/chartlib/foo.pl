
p(X,Y) :-
	test_equal(X,Y),
	q(X,Y).

q(_,_).


test_equal(X,Y) :-
	X==Y,
	!.
test_equal(X,Y) :-
	X\==Y,
	throw(error(chartlib,not_equal)).

chartlib_error_protect(G) :- 
	catch(G,E,chartlib_handle_error(E)).

chartlib_handle_error(error(chartlib,E)) :-
	display('Chartlib error: '),
	display(E),
	nl.
