%%-------------------------------------------------%%
%% FIPA Iterated Contract Net Interaction protocol %%
%%-------------------------------------------------%%
 
:- module(iterated_contract_net,_,[]).

role(initiator,'iterated_contract_net_initiator').
role(participant,'iterated_contract_net_participant').

init(initiator,cfp).

% answers( Role, Last Message recived, List of Conditions, List of Posible answers).

answers(participant,cfp,[],[not_understood,refuse,propose]).
answers(initiator,propose,[],[reject_proposal,accept_proposal,cfp]).
answers(participant,accept_proposal,[],[failure,inform]).

end(not_understood).
end(refuse).
end(reject_proposal).
end(failure).
end(inform).