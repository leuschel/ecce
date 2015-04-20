%%----------------------------------------------%%
%% FIPA Query Interaction protocol            %%
%%----------------------------------------------%%
 
:- module(query,_,[]).

role(initiator,'query_initiator').
role(participant,'query_participant').

init(initiator,query_if).
init(initiator,query_ref).

% answers( Role, Last Message recived, List of Conditions, List of Posible answers).

answers(participant,query_if,[],[not_understood,refuse,failure,inform]).
answers(participant,query_ref,[],[not_understood,refuse,failure,inform]).

end(not_understood).
end(refuse).
end(failure).
end(inform).