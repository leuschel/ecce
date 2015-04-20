%%----------------------------------------------%%
%% FIPA Request Interaction protocol Initiator  %%
%%----------------------------------------------%%
 
:- interface(request_initiator).

:- export([not_understood/12,refuse/12,agree/12]).
:- export([failure/12,inform/12]).
