:- module(send,[send/2],[actmods,hiord]).

:- use_module(library('actmods/webbased_locate')).
:- use_active_module(agent1, [agent1/2]).
:- use_active_module(agent2, [agent2/2]).

:- meta_predicate send(?,addmodule).

send(Sender, Message, I) :-
	Sender(I, Message).
