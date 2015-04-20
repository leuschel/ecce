
:- include(ho).

%% This defines the module. List/1 is >local<.
module(lists,[member:Member,append:Append]) :-

	( List([]) :- true ),
	( List([_|T]) :- List(T) ),
	
	( Member(X,[X|Y]) :- List(Y) ),
	( Member(X,[_|Y]) :- Member(X,Y) ),

	( Append([],Y,Y) :- List(Y) ),
	( Append([X|Y],Z,[X|W]) :- Append(Y,Z,W) ).


%% Note that the counter itself is local (inaccessible from the outside).
module(counter,[get:Counter,set:Set,inc:Inc]) :-

	Counter := 0,       

	( Set(X)  :- Counter := X ),

	( Inc(NX) :- Counter(X), NX is X+1, Counter := NX ).

main1 :-
 	module(lists,M), 
	M::member(X,[1,2,3]),
	write(X),
	fail.

main2 :-
	module(counter,M), 
	M::set(1),
 	M::get(X),
	write(X), nl,
	M::inc(_),
	M::get(NX),
	write(NX), nl.

%% Does not work well yet...
main3 :-
	module(counter,M), 
	M::set(1),
 	( M::get(X),
	  write(X), nl,
	  M::inc(_),
	  M::get(NX),
	  write(NX), nl,
	  fail
	; M::get(NNX),
	  write(NNX), nl ).
