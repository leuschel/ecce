%%transaction_test.pl
:- module(_,_,[expander]).
:- use_module(library(system), [pause/1]).
:- use_module(library(concurrency), [eng_call/4,eng_wait/1,eng_release/1]).
:- include(transactions).


:- data(balance/2). 

balance(john,   8000).
balance(peter, 12000).

:- transactional(calc_interest/0).
:- transactional(deposit/2).

calc_interest :- 
	balance(Person, Value), 
	pause(5), 
	NewValue is 0.1 * Value + Value, 
	retract_fact( balance(Person, _) ), 
	asserta_fact( balance(Person, NewValue) ),
	fail. %backtrack over all persons
calc_interest :-
	true.

deposit(Person, Value) :-
	balance(Person, OldValue), 
	NewValue is OldValue + Value,S
	retract_fact(balance(Person,_)), 
	asserta_fact(balance(Person, NewValue)).

two_calls(Call1, Call2) :-
	eng_call(Call1, create, create, T1),
	pause(2),
	eng_call(Call2, create, create, T2),
	eng_wait(T1),
	eng_release(T1),
	eng_wait(T2),
	eng_release(T2).


demo_txns :-
	display_string("Concurrently calculating 10% interest and"),
	nl,
	display_string("depositing $5000 on top of john's balance"),
	nl,
	display_string("of: $"), balance(john,X), display(X), nl,
	display_string("Resultant balance should be $"),
	X1 is X * 0.1 + X + 5000, display(X1),
	display_string(" or $"),
	X2 is ( (X + 5000) * 0.1) + X + 5000, display(X2),
	nl,
	display_string("Please Wait..."), nl,
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
	two_calls( calc_interest,deposit(john, 5000)),
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	display_string("Done - Result is: $"), balance(john,Y),
	display(Y), nl.
