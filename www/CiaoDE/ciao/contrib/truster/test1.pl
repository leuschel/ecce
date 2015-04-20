:- module(test1, _, [truster, expander]).

:- use_module(library(lists)).
:- use_module(library(sort)).

:- truster append([1,2,3],[4,5,6],X) -> append([1,2,3],[4,5,6],[1,2,3,4,5,6,7]).

:- truster append(X,[2],[1,2,3]).

:- truster \+(append(X,[2],[1,2])).

:- truster append(X,[2],[1,2]).

:- truster sort([1,2,6,5,2,1],X) -> sort([1,2,6,5,2,1],[1,2,5,6]).
