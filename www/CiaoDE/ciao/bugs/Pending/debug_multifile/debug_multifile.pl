:- module( _ , _ , _ ).

%% ?- multifile( p/0 ).
%%
%% yes
%% {trace}
%% ?- p.
%%
%% yes
%% {trace}
%% ?- 

:- multifile p/0.
p :- display('yupiuii\n'), fail.
