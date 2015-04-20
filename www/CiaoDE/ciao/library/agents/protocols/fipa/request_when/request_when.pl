%%----------------------------------------------%%
%% FIPA Request When Interaction protocol       %%
%%----------------------------------------------%%
 
:- module(request_when,_,[]).

role(initiator,'request_when_initiator').
role(participant,'request_when_participant').

init(initiator,request_when).

% answers( Role, Last Message recived, List of Conditions, List of Posible answers).

answers(participant,request_when,[],[not_understood,refuse,agree]).
answers(participant,request_when,[send(agree)],[refuse,failure,inform]).

end(not_understood).
end(refuse).
end(failure).
end(inform).