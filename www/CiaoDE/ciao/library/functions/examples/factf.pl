:- module(_,_,[functions]).

fact(0) := 1.  
fact(N) := N * ~fact(--N) :- N > 0.

%% fact(N) := N=0 ? 1
%%          | N>0 ? N * ~fact(--N).


