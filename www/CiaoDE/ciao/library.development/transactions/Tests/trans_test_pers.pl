%%transaction_test_pers.pl
:- module(_,_,[expander,persdb]).
:- use_module(library(system), [pause/1]).
:- use_module(library(concurrency), [eng_call/4,eng_wait/1,eng_release/1]).
:- include(transactions).

persistent_dir(db_test,'./').
:- persistent(balance/2,db_test).

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
	pause(5),
	NewValue is OldValue + Value,
	retract_fact(balance(Person,_)), 
	asserta_fact(balance(Person, NewValue)).

three_calls(Call1, Call2,Call3) :-
	eng_call(Call1, create, create, T1),
	pause(2),
	eng_call(Call2, create, create, T2),
	pause(3),
	eng_call(Call3, create, create, T3),
	eng_wait(T1),
	eng_release(T1),
	eng_wait(T2),
	eng_release(T2),
	eng_wait(T3),
	eng_release(T3).

initialize:-
	retract_fact(_),
	fail.
initialize.

demo_txns :-

	initialize,
        asserta_fact(balance(john,   8000)),
        asserta_fact(balance(peter, 12000)),
        asserta_fact(balance(jorge, 1000)),

% 	display_string("Concurrently calculating 10% interest and"),
% 	nl,
% 	display_string("depositing $5000 on top of john's balance"),
% 	nl,
% 	display_string("of: $"), balance(john,X), display(X), nl,
% 	display_string("Resultant balance should be $"),
% 	X1 is X * 0.1 + X + 5000, display(X1),
% 	display_string(" or $"),
% 	X2 is ( (X + 5000) * 0.1) + X + 5000, display(X2),
% 	nl,
% 	display_string("Please Wait..."), nl,

	display_string("Concurrently depositing $10000 on top of jorge's balance "),
	nl,
	display_string("depositing $5000 on top of john's balance"),
	nl,
	display_string("depositing $3000 on top of peter's balance"),
	nl,
	display_string("Resultant balance should be $"),

	display_string("jorge's balance of: $"), balance(jorge,X), display(X), nl,
	X1 is X + 10000, display(X1),nl,
	display_string("peter's balance of: $"), balance(peter,Y), display(Y), nl,
	Y1 is Y + 3000, display(Y1),nl,
	display_string("john's balance of: $"), balance(peter,Z), display(Z), nl,
	Z1 is Z + 5000, display(Z1),nl,


        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
	three_calls( deposit(jorge,10000),
	             deposit(john, 5000),
		     deposit(peter,3000)),
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	display_string("Done - jorge's balance: $"), balance(jorge,XR),
	display(XR), nl,
	display_string("Done - peter's balance: $"), balance(peter,YR),
	display(YR), nl,
	display_string("Done - john's balance: $"), balance(john,ZR),
	display(ZR), nl.
