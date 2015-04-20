%% ---------------------------------------------------------------------------
%
%    Véase:
%
% $ ciao-1.11
% {Including /home/boris/.ciaorc
% }
% Ciao-Prolog 1.11 #41: Thu Sep 18 19:18:58 CEST 2003
% ?- use_module(f).
%
% yes
% ?- give(X).
% a
% no
% ---------------------------------------------------------------------------

:- module(_, _, []).

:- export(main/0).
main :-
	give(X).

:- export(give/1).
give(X):-
        foo(X).

:- export(foo/1).
foo(a) :- fail.
foo(b).
