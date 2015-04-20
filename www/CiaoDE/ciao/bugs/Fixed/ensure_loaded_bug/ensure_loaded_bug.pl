%% ---------------------------------------------------------------------------
%
%    Véase:
%
% [boris@salmon tmp]$ ciao-1.11
% {Including /home/boris/.ciaorc
% }
% Ciao-Prolog 1.11 #41: Thu Sep 18 19:18:58 CEST 2003
% ?- ensure_loaded(f).
%
% yes
% ?- give(X).
% a
% no
% ---------------------------------------------------------------------------

foo(a).
foo(a).
foo(a).
foo(a).
foo(a).
foo(b).

give(X):-
        foo(X),
        display(X),
        fail.
