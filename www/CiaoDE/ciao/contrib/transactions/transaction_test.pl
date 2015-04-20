%%transaction_test.pl
%%(c)2003 Nick D. Pattengale
%% nickp@cs.unm.edu

:- module(transaction_test,_,[]).

%% The two goals to execute to see transactions
%% in action are demo_phantom_write/0 and
%% demo_txns/0

%% This file will load if it exists in the same directory
%% with all of the other files turned in with this project.

%% This demo pulls together the entire transaction
%% system.  The test will run two threads, one of which
%% will simulate a deposit into a bank account and
%% the other will simulate an interest calculation
%% program.  The test ensures a pause in the interest
%% calculation thread so that the update made by the
%% deposit thread is capable of being lost without
%% locking.


%If this module gets integrated into the ciao
%distribution, we will want to do the use_package...
%but that of course implies that this package exists
%in the ciao library...include for now

%:- use_package(transactions).
:- include(transactions).

%%the table of initial bank account balances
:- data(balance/2). 
balance(nick,   8000).
balance(manuel, 12000).

%%declare calc_interest as a transaction
:- transactional(calc_interest/0).

%%NOTICE that the definition of calc_interest
%%and dirty_calc_interest (just below) look the
%%same.  The only difference is that calc_interest
%%is declared a transaction.  The same is the case
%%with deposit/2
calc_interest :- 
	balance(Person, Value), 
	pause(5), 
	NewValue is 0.1 * Value + Value, 
	retract_fact( balance(Person, _) ), 
	assertz_fact( balance(Person, NewValue) ),
	fail. %backtrack over all persons
calc_interest :-
	true.

dirty_calc_interest :-
	balance(Person, Value), 
	pause(5),  
	NewValue is 0.1 * Value + Value,
	retract_fact( balance(Person, _) ),
	assertz_fact( balance(Person, NewValue) ),
	fail. %backtrack over all persons
dirty_calc_interest :-
	true.

:- transactional(deposit/2).

deposit(Person, Value) :-
	balance(Person, OldValue), 
	NewValue is OldValue + Value,
	retract_fact(balance(Person,_)), 
	assertz_fact(balance(Person, NewValue)).

dirty_deposit(Person, Value) :-
	balance(Person, OldValue), 
	NewValue is OldValue + Value,
	retract_fact(balance(Person,_)), 
	assertz_fact(balance(Person, NewValue)).


:- use_module(library(system), [pause/1]).
:- use_module(library(concurrency), [eng_call/4,eng_wait/1,eng_release/1]).

two_calls(Call1, Call2) :-
	eng_call(Call1, create, create, T1),
	pause(2),
	eng_call(Call2, create, create, T2),
	eng_wait(T1),
	eng_release(T1),
	eng_wait(T2),
	eng_release(T2).


demo_phantom_write :-
	display_string("Concurrently calculating 10% interest and"),
	nl,
	display_string("depositing $5000 on top of nick's balance"),
	nl,
	display_string("of: $"), balance(nick,X), display(X), nl,
	display_string("Resultant balance should be $"),
	X1 is X * 0.1 + X + 5000, display(X1),
	display_string(" or $"),
	X2 is ( (X + 5000) * 0.1) + X + 5000, display(X2),
	nl,
	display_string("Please Wait..."), nl,
	two_calls( dirty_calc_interest, dirty_deposit(nick, 5000) ),
	display_string("Done - Result is: $"), balance(nick,Y),
	display(Y), nl,
	display_string("Obviously transactions are necessary!"), nl.

demo_txns :-
	display_string("Concurrently calculating 10% interest and"),
	nl,
	display_string("depositing $5000 on top of manuel's balance"),
	nl,
	display_string("of: $"), balance(manuel,X), display(X), nl,
	display_string("Resultant balance should be $"),
	X1 is X * 0.1 + X + 5000, display(X1),
	display_string(" or $"),
	X2 is ( (X + 5000) * 0.1) + X + 5000, display(X2),
	nl,
	display_string("Please Wait..."), nl,
	two_calls( calc_interest, deposit(manuel, 5000) ),
	display_string("Done - Result is: $"), balance(manuel,Y),
	display(Y), nl.
