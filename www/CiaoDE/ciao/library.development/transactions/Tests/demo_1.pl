:- module(demo,
	[
	transactional_demo/0
	],
	[expander,persdb]).
:- use_module(library(system), [pause/1]).
:- use_module(library(concurrency), 
	[eng_call/4,
	 eng_wait/1,
	 eng_release/1
	]).
:- use_module(library(write)).
:- include(transactions).           

%% Persistence of balance/2
persistent_dir(db_test,'./db').
:- persistent(balance/2,db_test).


%% Declaration of transactional predicates
:- transactional(widthdraw/2).
:- transactional(deposit/2).

widthdraw(Person,Value):-
	balance(Person,OldValue),
	NewValue is OldValue - Value,
	retract_fact(balance(Person,_)), 
	asserta_fact(balance(Person, NewValue)).

deposit(Person, Value) :-
	balance(Person, OldValue), 
	NewValue is OldValue + Value,
	retract_fact(balance(Person,_)), 
	asserta_fact(balance(Person, NewValue)).

transactional_demo:-
	initialize_accounts,
	
	display_string("Concurrently depositing $3000 "),nl,
	display_string("on top of Bill Gates' balance and "),nl,
	display_string("widthdrawing $2000 on it."),nl,

        balance('Bill Gates',X), 
% 	display_string("Resultant balance should be $"), 
% 	display_string
% 	X1 is X + 1000,
% 	X2 is X 
 
	display_string(" or $"),
	display_string("Please Wait..."), nl,
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
        % Execution concurrently of transactional predicates
	two_calls( widthdraw('Bill Gates',2000), 
	           deposit('Bill Gates, 3000)), 
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	display_string("Done - Result is: $"), balance('Bill Gates',Y),
	display(Y), nl.



two_calls(Call1, Call2) :-
	eng_call(Call1, create, create, T1),
	pause(2),
	eng_call(Call2, create, create, T2),
	eng_wait(T1),
	eng_release(T1),
	eng_wait(T2),
	eng_release(T2).

initialize_accounts:-
	retract_fact(_),
        asserta_fact(balance('Bill Gates',   8000)),
        asserta_fact(balance('Rockefeller', 12000)),
initialize_accounts.
