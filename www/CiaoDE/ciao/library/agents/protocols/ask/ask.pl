%%----------------------------------------------%%
%% FIPA Request Interaction protocol            %%
%%----------------------------------------------%%
 
:- module(ask,_,[]).
%:- inherit_class(library('agents/protocols')).


role(initiator,'query-initiator').
role(participant,'query-participant').

query:-
	protocols.

init(query-if).
init(query-ref).

answers(query-if,[],[not-understood,refuse,failure,inform]).
answers(query-ref,[],[not-understood,refuse,failure,inform]).

end(not-understood).
end(refuse).
end(failure).
end(inform).