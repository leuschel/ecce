
:- include(ho).

%% This defines the module. List/1 is >local<.
module(lists,[Member,Append]) :-

	( List([]) :- true ),
	( List([_|T]) :- List(T) ),
	
	( Member(X,[X|Y]) :- List(Y) ),
	( Member(X,[_|Y]) :- Member(X,Y) ),

	( Append([],Y,Y) :- List(Y) ),
	( Append([X|Y],Z,[X|W]) :- Append(Y,Z,W) ).

%% This uses the module. 
main(X) :-
	module(lists,[Member,_Append]), 
	Member(X,[1,2,3]).
