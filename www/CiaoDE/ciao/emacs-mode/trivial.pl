:- module(_,[app/3],[assertions]).
%% :- module(_,[lst/1,app/3],[assertions]).

%% :- entry lst/1 : list.
%% 
%% lst([]).
%% lst([_|X]) :- lst(X).

:- entry app/1 : list * list * list.

app([],X,X) :- lst(X).
app([X|Y],Z,[X|W]) :- app(Y,Z,W).
