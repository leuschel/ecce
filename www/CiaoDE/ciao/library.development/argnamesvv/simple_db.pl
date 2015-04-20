:- module(simple_db,_,[assertions,regtypes,argnamesv]).
:- use_module(library(aggregates)).

:- comment(title,"A simple database application using argument names").

:- pred product/4 :: int * string * string * int.

:- argnames 
product( id,    description,    brand,          quantity        ).
%       ----------------------------------------------------------
% TODO: this does not work because the head is in bad place...
product${Id=>0, description=>"Nothing", brand=>"None", quantity => 0} :- Id = id.
product(  1,    "Keyboard",     "Logitech",     6               ).
product(  2,    "Mouse",        "Logitech",     5               ).
product(  3,    "Monitor",      "Philips",      3               ).
product(  4,    "Laptop",       "Dell",         4               ).

:- argnames 
product2( id,    description,    brand,          quantity        ).

get(A, B) :-
	display(product$argnames), nl,
	product${A=>B}.

rec(A) :-
	A = product${id=>product2${}}.

% Compute the stock of products from a given brand.
% Note call to findall is equivalent to: findall(Q,product(_,_,Brand,Q),L).
brand_stock(Brand,Stock) :-
        findall(Q,product${brand=>Brand,quantity=>Q},L),
        sumlist(L,Stock).

sumlist([],0).
sumlist([X|T],S) :- 
        sumlist(T,S1),
        S is X + S1.
