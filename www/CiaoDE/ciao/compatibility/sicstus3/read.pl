:- module(read,[read_term/3]).

read_term(X,Y,Z) :- user:read_term(X,Y,Z).

%% %% Actually, these are incompatible (because of the dictionary 
%% %% representation) and in any case non-standard.

%% % SICStus1.8 etc.
%% % read_top_level(S,X,Y) :- 'SYSCALL'(read_top_level(S,X,Y)).
%% % SICStus2.x and SICStus3
%% read_top_level(S,X,Y) :- prolog:read_top_level(S,X,Y).
