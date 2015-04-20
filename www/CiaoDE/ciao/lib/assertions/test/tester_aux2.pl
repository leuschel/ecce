:- module(tester_aux2,[current_host/1],[assertions,isomodes,regtypes]).

% :- true regtype current_host(?atm).
% :- true regtype current_host(?atm) + iso.

:- pred current_host(?atm) + iso.

current_host(foo).
current_host(bar).
