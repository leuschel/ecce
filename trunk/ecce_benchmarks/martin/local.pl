

:- module(local, [unfold/3]).
:- use_module(library(lists)).

:- dynamic trace_term_list/2.

rtest(N, ChTree, Time):-
    length(L, N),
    append(L, _, L1),
    statistics(runtime, _),
    unfold(rev:rev(L1, _), ChTree),
    statistics(runtime, [_, Time]).

ttest(ChTree, Time):-
    pd_goal(transpose, Goal),
    statistics(runtime, _),
    unfold(Goal, ChTree),
    statistics(runtime, [_, Time]).

%pd_goal(transpose, transpose:transpose([[X1,X2,X3,X4,X5,X6,X7,X8,X9],
%                                        Xr2,Xr3],Xtrm)).

unfold(Goal, ChTree):-
%    statistics(runtime, _),
    abolish(trace_term_list/2),
    assert(trace_term_list(Tail, Tail)),
    unfold1(Goal, ChTree).
%    statistics(runtime, [_, Time]), write('time: '), write(Time), nl.

unfold1(Goal, _ChTree):-
    call(Goal, Trace),
%       write('trace:  '), write(Trace), nl,
    retract(trace_term_list(TraceTerms, [Trace|TraceTail])),
    assert(trace_term_list(TraceTerms, TraceTail)),
    fail.
unfold1(_, ChTree):-
    trace_term_list(TraceTerms, []),
    convert_trace_to_chtree(TraceTerms, [_], 1, ChTree).

:- block call(-,?).

call([], []):-
    !.
call([Goal|Goals], [Trace|Traces]):-
    !,
    call(Goal, Trace),
    call(Goals, Traces).
call((Goal1,Goal2), (Trace1,Trace2)):-
    !,
    call(Goal1, Trace1),
    call(Goal2, Trace2).
call(Goal, Trace):-
    get_module(Goal, Module, Atom),
    Atom =.. List,
    append(List, [Trace], List1),
    NewAtom =.. List1,
    call(Module:NewAtom).



/* convert_to_chpath(trace term,
                     current literal position,
                     final literal position,
                     characteristic path,
                     path end).
*/

convert_to_chpath(V, Pos, Pos1, Path, Path):-
    var(V), !,
    Pos1 is Pos + 1.
%convert_to_chpath(Atom, Pos, Pos, select(Pos, [match(Atom, PathIn)]), PathIn):-
%     atom(Atom), !.
convert_to_chpath('=..', Pos, Pos, built_in_eval(Pos, '=..', Path), Path):-
    !.
convert_to_chpath(is, Pos, Pos, built_in_eval(Pos, is, Path), Path):-
    !.
convert_to_chpath('>', Pos, Pos, built_in_eval(Pos, '>', Path), Path):-
    !.
convert_to_chpath('>=', Pos, Pos, built_in_eval(Pos, '>=', Path), Path):-
    !.
convert_to_chpath('<', Pos, Pos, built_in_eval(Pos, '<', Path), Path):-
    !.
convert_to_chpath('=<', Pos, Pos, built_in_eval(Pos, '=<', Path), Path):-
    !.
convert_to_chpath(trace(Num, A, B), PosIn, PosOut, select(PosIn, [match(Num, Path)]), PathEnd):-
    !,
    convert_to_chpath(A, PosIn, Pos1, Path, Path1),
    convert_to_chpath(B, Pos1, PosOut, Path1, PathEnd).
convert_to_chpath(trace(Num, A), PosIn, PosOut, select(PosIn, [match(Num, Path)]), PathEnd):-
    !,
    convert_to_chpath(A, PosIn, PosOut, Path, PathEnd).
convert_to_chpath(N, Pos, Pos, select(Pos, [match(N, PathEnd)]), PathEnd):-
    integer(N), !.
convert_to_chpath((Trace1, Trace2), PosIn, PosOut, Path, PathEnd):-
    !,
    convert_to_chpath(Trace1, PosIn, Pos1, Path, Path1),
    convert_to_chpath(Trace2, Pos1, PosOut, Path1, PathEnd).
convert_to_chpath(Term, PosIn, PosOut, select(PosIn, [match(Num, Path)]), PathEnd):-
    Term =.. [trace, Num|Args],
    convert_list_to_chpath(Args, PosIn, PosOut, Path, PathEnd).

convert_list_to_chpath([], Pos, Pos, Path, Path).
convert_list_to_chpath([Arg|Args], PosIn, PosOut, Path, PathEnd):-
    convert_to_chpath(Arg, PosIn, Pos1, Path, Path1),
    convert_list_to_chpath(Args, Pos1, PosOut, Path1, PathEnd).





/* convert_trace_to_chtree */



% Traces = [_|_] i.e. the trace tree is non empty.

convert_trace_to_chtree(Traces, ElimList, Index, ChTree):-
    ((Index == success ; Index == stop) ->
        ChTree = Index
    ;
        get_traces(Traces, Index, ElimList, ChTree)
    ).

get_traces([Arg|Args], Index, ElimList, ChTree):-
    next_trace([Arg|Args], Index, ClauseNr, NewArgs, RemainingArgs, ElimList, NewElimList),
    build_chtree(ClauseNr, Index, ElimList, NewArgs, RemainingArgs, NewElimList, ChTree).

% Quick hack fix.  Only works for builtins of arity 2.

build_chtree(Builtin, Index, _, Traces, [], ElimList, built_in_eval(Index, pred(Builtin, 2), ChTree)):-
    builtin(Builtin),
    !,
    get_selected_literal_index(ElimList, 1, Index2),
    convert_trace_to_chtree(Traces, ElimList, Index2, ChTree).

build_chtree(ClauseNr, Index, _, NewArgs, [], ElimList2, select(Index, [match(ClauseNr, ChTree)])):-
    !,
    get_selected_literal_index(ElimList2, 1, Index2),
    convert_trace_to_chtree(NewArgs, ElimList2, Index2, ChTree).

build_chtree(ClauseNr, Index, ElimList, NewArgs, RemainingArgs, _, select(Index, [match(ClauseNr, ChTree)|MatchList])):-
%    RemainingArgs = [_|_],
    eliminate_indices(NewArgs, ElimList2),
    get_selected_literal_index(ElimList2, 1, Index2),
    convert_trace_to_chtree(NewArgs, ElimList2, Index2, ChTree),
    get_rest_traces(RemainingArgs, Index, ElimList, MatchList).

list_equal([], []).
list_equal([X|Y], [W|Z]):-
        (var(X), var(W)) ; (nonvar(X), nonvar(W)),
        list_equal(Y, Z).

next_trace([], _, _, [], [], _, _).
next_trace([Arg|Args], Index, Num, NewArgs, RemainingArgs, ElimList, NewElimList):-
    get_selected_literal(Index, Arg, ClauseNr, NewArg, ElimList, NewElimList2),
    (ClauseNr = Num ->
        NewElimList = NewElimList2,
        NewArgs = [NewArg|NewArgs1],
        next_trace(Args, Index, ClauseNr, NewArgs1, RemainingArgs, ElimList, NewElimList)
    ;
        NewArgs = [],
        RemainingArgs = [Arg|Args]
    ).

get_rest_traces([], _, _, []).
get_rest_traces([Arg|Args], Index, ElimList, [match(ClauseNr, ChTree)|MatchList]):-
    next_trace([Arg|Args], Index, ClauseNr, NewArgs, RemainingArgs, ElimList, _),
    eliminate_indices(NewArgs, ElimList2),
    get_selected_literal_index(ElimList2, 1, Index2),
    convert_trace_to_chtree(NewArgs, ElimList2, Index2, ChTree),
    get_rest_traces(RemainingArgs, Index, ElimList, MatchList).

get_selected_literal(1, [T|Ts], ClauseNr, NewArg, [_|Es], NewElimList):-
    !,
    get_literal(T, Ts, ClauseNr, NewArg, Es, NewElimList).
get_selected_literal(2, [T1, T|Ts], ClauseNr, [T1|NewArg], [E1, _|Es], [E1|NewElimList]):-
    !,
    get_literal(T, Ts, ClauseNr, NewArg, Es, NewElimList).
get_selected_literal(3, [T1, T2, T|Ts], ClauseNr, [T1, T2|NewArg], [E1, E2, _|Es], [E1, E2|NewElimList]):-
    !,
    get_literal(T, Ts, ClauseNr, NewArg, Es, NewElimList).
get_selected_literal(4, [T1, T2, T3, T|Ts], ClauseNr, [T1, T2, T3|NewArg], [E1, E2, E3, _|Es], [E1, E2, E3|NewElimList]):-
    !,
    get_literal(T, Ts, ClauseNr, NewArg, Es, NewElimList).
get_selected_literal(5, [T1, T2, T3, T4, T|Ts], ClauseNr, [T1, T2, T3, T4|NewArg], [E1, E2, E3, E4, _|Es], [E1, E2, E3, E4|NewElimList]):-
    !,
    get_literal(T, Ts, ClauseNr, NewArg, Es, NewElimList).
get_selected_literal(Index, [T1, T2, T3, T4, T5|Ts], ClauseNr, [T1, T2, T3, T4, T5|NewArg], [E1, E2, E3, E4, E5|Es], [E1, E2, E3, E4, E5|NewElimList]):-
    Index1 is Index - 5,
    get_selected_literal(Index1, Ts, ClauseNr, NewArg, Es, NewElimList).


get_selected_literal_index([], _, success).
get_selected_literal_index([E|Es], N, Index):-
    get_selected_literal_index4([E|Es], N, Index).
get_selected_literal_index4([], _, stop).
get_selected_literal_index4([E|Es], N, Index):-
    (var(E) ->
        Index = N
    ;
        N1 is N + 1,
        get_selected_literal_index4(Es, N1, Index)
    ).

% The list of clause numbers is in ascending order since the clauses in the
% program are numbered top-down and the search is performed top-down

get_literal(trace(ClauseNr, Arg), Ts, ClauseNr, [Arg|Ts], Es, ElimList):-
    !,
    get_elim_list([Arg], Es, ElimList).
get_literal(trace(ClauseNr, A1, A2), Ts, ClauseNr, [A1, A2|Ts], Es, ElimList):-
    !,
    get_elim_list([A1, A2], Es, ElimList).
get_literal(trace(ClauseNr, A1, A2, A3), Ts, ClauseNr, [A1, A2, A3|Ts], Es, ElimList):-
    !,
    get_elim_list([A1, A2, A3], Es, ElimList).
get_literal(trace(ClauseNr, A1, A2, A3, A4), Ts, ClauseNr, [A1, A2, A3, A4|Ts], Es, ElimList):-
    !,
    get_elim_list([A1, A2, A3, A4], Es, ElimList).
get_literal(trace(ClauseNr, A1, A2, A3, A4, A5), Ts, ClauseNr, [A1, A2, A3, A4, A5|Ts], Es, ElimList):-
    !,
    get_elim_list([A1, A2, A3, A4, A5], Es, ElimList).
get_literal(trace(ClauseNr, A1, A2, A3, A4, A5, A6), Ts, ClauseNr, [A1, A2, A3, A4, A5, A6|Ts], Es, ElimList):-
    !,
    get_elim_list([A1, A2, A3, A4, A5, A6], Es, ElimList).
get_literal(trace(ClauseNr, A1, A2, A3, A4, A5, A6, A7), Ts, ClauseNr, [A1, A2, A3, A4, A5, A6, A7|Ts], Es, ElimList):-
    !,
    get_elim_list([A1, A2, A3, A4, A5, A6, A7], Es, ElimList).
get_literal(T, Ts, T, Ts, Es, Es):-
    (integer(T), !) 
     ;
    (builtin(T), !).
get_literal(T, Ts, ClauseNr, NewArg, Es, ElimList):-
    T =.. [trace, ClauseNr | Args],
    !,
    get_elim_list(Args, Es, ElimList),
    append(Args, Ts, NewArg).

get_elim_list([], T, T).
get_elim_list([Arg|Args], Tail, ElimList):-
    (var(Arg) ->
        ElimList = [not_selected|Es]
    ;
        ElimList = [_|Es]
    ),
    get_elim_list(Args, Tail, Es).



get_literal(trace(ClauseNr, Arg), Ts, ClauseNr, [Arg|Ts]):- !.
get_literal(trace(ClauseNr, A1, A2), Ts, ClauseNr, [A1, A2|Ts]):- !.
get_literal(trace(ClauseNr, A1, A2, A3), Ts, ClauseNr, [A1, A2, A3|Ts]):- !.
get_literal(trace(ClauseNr, A1, A2, A3, A4), Ts, ClauseNr, [A1, A2, A3, A4|Ts]):- !.
get_literal(trace(ClauseNr, A1, A2, A3, A4, A5), Ts, ClauseNr, [A1, A2, A3, A4, A5|Ts]):- !.
get_literal(trace(ClauseNr, A1, A2, A3, A4, A5, A6), Ts, ClauseNr, [A1, A2, A3, A4, A5, A6|Ts]):- !.
get_literal(trace(ClauseNr, A1, A2, A3, A4, A5, A6, A7), Ts, ClauseNr, [A1, A2, A3, A4, A5, A6, A7|Ts]):- !.
get_literal(T, Ts, T, Ts):-
    (integer(T), !) 
     ;
    (builtin(T), !).
get_literal(T, Ts, ClauseNr, NewArg):-
    T =.. [trace, ClauseNr | Args],
    !,
    append(Args, Ts, NewArg).

builtin(is).
builtin('>').
builtin('>=').
builtin('<').
builtin('=<').
builtin('=..').
builtin('\\==').
builtin('=').


% eliminate_indices(trace_tree, elim_list).
%
% Takes a trace tree represented as a list of trace terms
% and finds the selected literal at the root of the tree
%
% Each trace term in the list begins matching the same
% clause number.

eliminate_indices([], _).
eliminate_indices([T|Ts], ElimList):-
    eliminate_indices1(T, ElimList),
    eliminate_indices(Ts, ElimList).

eliminate_indices1([], []).
eliminate_indices1([X|Y], ElimList):-
    get_elim_list([X|Y], ElimList).

get_elim_list([], []).
get_elim_list([Arg|Args], ElimList):-
    (var(Arg) ->
        ElimList = [not_selected|Es]
    ;
        ElimList = [_|Es]
    ),
    get_elim_list(Args, Es).


get_module(Pre:Suf, Mod, Atom):-
    get_module(Suf, Pre, Mod, Atom).

get_module(Pre:Suf, Pre1, Mod, Atom):-
    !,
    get_module(Suf, Pre1:Pre, Mod, Atom).
get_module(Atom, Mod, Mod, Atom).

%---------------------------------------------------------------------

% Used by local unfolding modules

max(X, Y, Max):-
    X > Y,
    Max = X.
max(X, Y, Max):-
    X =< Y,
    Max = Y.

%-----------------------------------------------------------------
%
% Builtins

:- block '=..'(-,-,?,?).

'=..'(Atom, Functor, Args, '=..'):-
    Atom =.. [Functor|Args].

'\=='(X, Y, _):-
    X == Y,
    !,
    fail.
'\=='(X, Y, Trace):-
    neq(X, Y, Trace).

:- block neq(-,?,?),neq(?,-,?).

neq(X, Y, Trace):-
    functor(X, FunX, ArityX),
    functor(Y, FunY, ArityY),
    (FunX == FunY ->
        (ArityX == ArityY ->
            when(ground(X), when(ground(Y), (X \== Y, Trace = '\\==')))
        ;
            Trace = '\\=='
        )
    ;
        Trace = '\\=='
    ).

% '\=='(X, Y, Trace):-
%     when(ground(X), when(ground(Y), (X \== Y, Trace = '\\=='))).


% Arithmetic
% ----------

'>'(X, Y, Trace):-
    when(ground(X), when(ground(Y), (X > Y, Trace = '>'))).

'>='(X, Y, Trace):-
    when(ground(X), when(ground(Y), (X >= Y, Trace = '>='))).

'<'(X, Y, Trace):-
    when(ground(X), when(ground(Y), (X < Y, Trace = '<'))).

'=<'(X, Y, Trace):-
    when(ground(X), when(ground(Y), (X =< Y, Trace = '=<'))).

is(X, Y, Trace):-
    when(ground(Y), (X is Y, Trace = is)).


