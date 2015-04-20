%%-------------------------------------------%%
%% FIPA English Auction Interaction protocol %%
%%-------------------------------------------%%
 
:- module(english_auction,_,[]).

role(initiator,'english_auction_initiator').
role(participant,'english_auction_participant').

init(initiator,inform).

% answers( Role, Last Message recived, List of Conditions, List of Posible answers).

answers(initiator,[],[send(inform)],[cfp]).
answers(participant,cfp,[],[not_understood,propose]).
answers(initiator,propose,[],[reject_proposal,accept_proposal]).
answers(initiator,propose,[send(accept_proposal)],[cfp]).
answers(initiator,propose,[],[inform]).
answers(initiator,propose,[send(accept_proposal)],[request]).


end(not_understood).
end(request).
end(inform).