:- use_package(hiord).

module(baz, [Q]) :-

	Q = (''(X) :- display(X)).

module(lists, [List]) :-

	List = (''(X) :- ( X=[] ; list1(X) )).

% 1
%	List = (''(X) :- ( X=[] ; List(X) )).

% 2
%	( List(X) :- ( X=[] ; List(X) ) ).

module(bar, [P]) :-
	
	module(baz, [Q]),
	
        R = (''(X) :- X>1 ),

	P = (''(X) :- Q(X), R(X) ).
        
list1([_]).
