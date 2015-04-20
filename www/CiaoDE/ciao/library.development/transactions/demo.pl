:- module(demo,_,
	[expander,persdb]).
:- use_module(library(system), [pause/1]).
:- use_module(library(concurrency), 
	[eng_call/4,
	 eng_wait/1,
	 eng_release/1
	]).
:- include(transactions).  

persistent_dir(db,'./db').
:- persistent(balance/2,db).

%----------------------------------------------------------------------
% Transactional version                                               |
%----------------------------------------------------------------------

%% Declaration of transactional predicates

:- transactional(calc_interest/0).
:- transactional(deposit/2).


% Calculate 10% interest and add this amount to 
% the previous balance (for each person)
calc_interest :- 
	balance(Person, Value), 
	pause(5), 
	NewValue is 0.1 * Value + Value, 
	retract_fact( balance(Person, _) ), 
	asserta_fact( balance(Person, NewValue) ),
	fail. %backtrack over all persons
calc_interest :-
	true.

% Deposit an amount (value) into person's account
deposit(Person, Value) :-
	balance(Person, OldValue), 
	NewValue is OldValue + Value,
	retract_fact(balance(Person,_)), 
	asserta_fact(balance(Person, NewValue)).

transactional_demo:-
	initialize_accounts,
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
        % Execution concurrently of transactions 
	two_calls( calc_interest, deposit(john, 5000)), 
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	display_string("Done - Result is: $"), balance(john,Y),
	display(Y), nl.

%----------------------------------------------------------------------
% Non transactional version                                           |
%----------------------------------------------------------------------

% Calculate 10% interest and add this amount to 
% the previous balance (for each person)
non_trans_calc_interest :- 
	balance(Person, Value), 
	pause(5), 
	NewValue is 0.1 * Value + Value, 
	retract_fact( balance(Person, _) ), 
	asserta_fact( balance(Person, NewValue) ),
	fail. %backtrack over all persons
non_trans_calc_interest :-
	true.

% Deposit an amount (value) into person's account
non_trans_deposit(Person, Value) :-
	balance(Person, OldValue), 
	NewValue is OldValue + Value,
	retract_fact(balance(Person,_)), 
	asserta_fact(balance(Person, NewValue)).

non_transactional_demo:-
	initialize_accounts,
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
        % Execution concurrently of transactions 
	two_calls( non_trans_calc_interest, non_trans_deposit(john, 5000)), 
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	display_string("Done - Result is: $"), balance(john,Y),
	display(Y), nl,
	display_string("Obviously transactions are necessary!"), nl.


%----------------------------------------------------------------------
% Auxiliary predicates                                                |
%----------------------------------------------------------------------

two_calls(Call1, Call2) :-
	eng_call(Call1, create, create, T1),
	pause(2),
	eng_call(Call2, create, create, T2),
	eng_wait(T1),
	eng_release(T1),
	eng_wait(T2),
	eng_release(T2).

initialize_accounts:-
	( balance(john,_) ->
	  retract_fact(balance(john,_))
        ;
	  true
	),
	( balance(peter,_) ->
	  retract_fact(balance(peter,_))
        ;
	  true  
        ),
	asserta_fact(balance(john,   8000)),	
	asserta_fact(balance(peter,   12000)).
