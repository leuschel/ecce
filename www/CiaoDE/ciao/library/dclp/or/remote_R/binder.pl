:- module(binder, 
	          [address/2, 
		    add_address/2, 
		    remove_address/1, 
		    get_all_agencies/1], []).

% :- use_package(persdb).
% persistent_dir(binder_db,'./binder_db').
% :- persistent(a/2,binder_db).

:- use_module(library(aggregates)).

:- data address/2.

add_address(manager, a(Host, Port)) :-
	address(manager, a(Host, Port)), !.
add_address(agency, a(Host, Port)) :-
	address(agency, a(Host, Port)), !.
add_address(Type, a(Host, Port)) :-
	atom(Host),
	number(Port),
	(Type==agency; Type==manager),
	asserta_fact(address(Type, a(Host, Port))).

remove_address(a(Host, Port)) :-
	retract_fact(address(_, a(Host, Port))),
	fail.
remove_address(_) .

get_all_agencies(L) :-
	findall(a(Host, Port), address(agency, a(Host, Port)), L).
