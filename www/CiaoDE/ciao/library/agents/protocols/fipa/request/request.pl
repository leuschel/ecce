%%----------------------------------------------%%
%% FIPA Request Interaction protocol            %%
%%----------------------------------------------%%

:- module(request,_,[]).

role(initiator,'request_initiator').
role(participant,'request_participant').

init(initiator,request).

% answers( Role, Last Message recived, List of Conditions, List of Posible answers).

answers(participant,request,[],[not_understood,refuse,agree]).
answers(participant,request,[send(agree)],[failure,inform]).

end(not_understood). end(refuse). end(failure). end(inform).

:- mine.

\mineadf a.


:sadf

 :-adf. \asdf
