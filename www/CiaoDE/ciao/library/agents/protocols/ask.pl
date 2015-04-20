%%----------------------------------------------%%
%% FIPA Request Interaction protocol            %%
%%----------------------------------------------%%
 
:- module(ask,_,[]).


role(asker,'ask_asker').
role(answer,'ask_answer').

init(ask).

answers(ask,[],[inform]).
answers(inform,[],[ask,no_more_ask]).

end(no_more_ask).
