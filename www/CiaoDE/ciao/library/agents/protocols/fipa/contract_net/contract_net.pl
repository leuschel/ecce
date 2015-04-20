%%----------------------------------------------%%
%% FIPA Contract Net Interaction protocol       %%
%%----------------------------------------------%%
 
:- module(contract_net,_,[]).
:- use_package(library('agents/protocols/generic')).
:- use_package(library('agents/messages')).

role(initiator,'contract_net_initiator').
role(participant,'contract_net_participant').

init(initiator,cfp).

% answers( Role, Last Message recived, List of Conditions, List of Posible answers).

answers(participant,cfp,[],[not_understood,refuse,propose]).
answers(initiator,propose,[],[reject_proposal,accept_proposal]).
answers(participant,accept_proposal,[],[failure,inform]).

end(not_understood).
end(refuse).
end(reject_proposal).
end(failure).
end(inform).