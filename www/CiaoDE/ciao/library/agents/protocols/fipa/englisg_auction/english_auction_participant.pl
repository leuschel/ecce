%%--------------------------------------------------------%%
%% FIPA English Auction Interaction protocol Participant  %%
%%--------------------------------------------------------%%
 
:- interface(english_auction_participant).

:- export([inform/12,cfp/12]).
:- export([reject_proposal/12,accept_proposal/12]).
:- export([request/12]).