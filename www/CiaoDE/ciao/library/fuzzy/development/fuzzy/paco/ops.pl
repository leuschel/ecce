:- op(1200,xfx,':~').   % Fuzzy Rule
:- op(1200,xf,':~').    % Fuzzy Fact

:- op(1200,xfx,':=').   % simple fuzzy prolog
:- op(1200,xf,':=').    % simple fuzzy prolog

:- op(1200,xfx,':#').   % definition of fuzzy predicate neg agreg etc
:- op(1175,fx,(=>)).    % implicacion fuzzy.
:- op(1150,fx,'fnot').  % fuzzy negation

:- op(1150, fx,aggr).   % declared aggregator

:- op(1190,fx,'min').   % fuzzy minimun
:- op(1190,fx,'luka').  % fuzzy luka
:- op(1190,fx,'prod').  % fuzzy prod

:- op(1190,fx,'max').   % fuzzy max
:- op(1190,fx,'dluka'). % fuzzy dluka
:- op(1190,fx,'dprod'). % fuzzy dprod

:- op(1150,fx,'fuzzy'). % fuzzied
:- op(1190,fx,'fuzzy_predicate'). 
:- op(1190,fx,'fuzzy_discrete').



% :- op(1200,xfx,':~').   % minimum
% :- op(1200,xf,':~').    % minimum

% :- op(1200,xfx,':*').   % prod
% :- op(1200,xf,':*').    % prod

% :- op(1200,xfx,':+').  %  luka...
% :- op(1200,xf,':+').  %  luka...

% :- op(1200,xfx,':=').   % simple fuzzy prolog
% :- op(1200,xf,':=').    % simple fuzzy prolog

% :- op(1200,xfx,':#').   % definition of neg agreg etc
% :- op(1150,fx,'fnot').  % fuzzy negation

%:- op(1195,fx,'aggr').  % fuzzy aggregation

%  :- op(1150,fx,'aggr_max').
%  :- op(1150,fx,'aggr_dluka').
%  :- op(1150,fx,'aggr_dprod').
%  :- op(1150,fx,'aggr_min').
%  :- op(1150,fx,'aggr_luka').
%  :- op(1150,fx,'aggr_prod').
