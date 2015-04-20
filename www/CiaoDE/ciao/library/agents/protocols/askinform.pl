%%----------------------------------------------%%
%% FIPA Request Interaction protocol            %%
%%----------------------------------------------%%
 
:- class(askinform).
%:- inherit_class(library('agents/protocols')).
:- use_package(library('agents/messages')).

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