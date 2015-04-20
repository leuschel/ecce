:- module(_,_,[assertions,functions]).

:- pred fact(+int,-int) + is_det.
:- pred fact(-int,+int) + non_det.

fact(0) := 1.  
fact(N) := N * ~fact(--N) :- N > 0.



