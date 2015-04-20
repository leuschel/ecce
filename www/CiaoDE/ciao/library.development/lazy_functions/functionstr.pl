:- module(functionstr, [defunc/3, defunc_goal/3], []).
:- include(ops).

:- use_module(library(terms), [copy_args/3]).
:- use_module(library(write)).

:- data function/2.
:- data ignore_arith/1.

% defunc(FuncItem, PredItem, Module) :- PredItem is a clause, query or
% command equivalent to FuncItem but without functions.
% To be called as a sentence translation


defunc((?- _), _, _) :- !, fail.
defunc((:- function(Spec)), _, Mod) :- !,
        ( Spec = F/A, functor(P, F, A) ->
            make_function(P, Mod)
        ; Spec = arith(true) ->
            retractall_fact(ignore_arith(Mod))
        ; Spec = arith(false) ->
            asserta_fact(ignore_arith(Mod))
        ; inform_user(['Invalid function specification: ',Spec])
        ).
defunc((:- _), _, _) :- !, fail.
defunc(end_of_file, end_of_file, Mod) :- !,
        retractall_fact(function(_,Mod)),
        retractall_fact(ignore_arith(Mod)).
defunc((FuncHead := FuncValue), (Head :- Body), Mod) :- !,
        arith_flag(Mod, Arith_Flag),
        defunc_head(FuncHead, Mod, Arith_Flag, NewFuncHead, AddBody, RestBody),
        defunc_exp(FuncValue, Mod, Arith_Flag, NewFuncValue, RestBody, true),
        add_to_term(NewFuncHead, NewFuncValue, Head),
        del_last_true(AddBody, Body),
	write((Head :- Body)), nl.
defunc((FuncHead := FuncValue :- FuncBody), (Head :- Body), Mod) :- !,
        arith_flag(Mod, Arith_Flag),
        defunc_head(FuncHead, Mod, Arith_Flag, NewFuncHead, AddBody, FuncBody),
        defunc_exp(FuncValue, Mod, Arith_Flag, NewFuncValue, LastBody, true),
        add_to_term(NewFuncHead, NewFuncValue, Head),
        concat_bodies(AddBody, LastBody, Body),
	write((Head :- Body)), nl.
defunc((Head :- Body), (NewHead :- NewBody), Mod) :- !,
        arith_flag(Mod, Arith_Flag),
        defunc_head(Head, Mod, Arith_Flag, NewHead, NewBody, Body).
defunc(Head, (NewHead :- NewBody), Mod) :-
        arith_flag(Mod, Arith_Flag),
        defunc_head(Head, Mod, Arith_Flag, NewHead, Body, true),
        del_last_true(Body, NewBody).

arith_flag(Mod, Arith_Flag) :-
        ignore_arith(Mod) -> Arith_Flag = false ; Arith_Flag = true.

% defunc_head(Head, Module, NewHead, AddBody, RestBody) :- NewHead is a
% clause head without functions equivalent to Head when adding AddBody
% minus RestBody to the body of the clause.

defunc_head(Head, Module, Arith_Flag, NewHead, AddBody, RestBody) :-
        functor(Head, F, A),
        functor(NewHead, F, A),
        defunc_args(A, Head, Module, Arith_Flag, NewHead, AddBody, RestBody).

% defunc_args(N, Term, Module, Arith_Flag, NewTerm, AddGoal, RestGoal) :-
% NewTerm is a term without functions in its first N arguments, and this
% arguments are equivalent to the ones of Term when adding goals AddGoal
% minus RestGoal. Arith_Flag is 'true' if we want extract arithmetic functors.

defunc_args(0, _, _, _, _, X, X) :- !. 
defunc_args(N, T0, Mod, Arith, T1, Add, Rest) :-
        arg(N, T0, A0),
        arg(N, T1, A1),
        N1 is N-1,
        defunc_exp(A0, Mod, Arith, A1, NRest, Rest),
        defunc_args(N1, T0, Mod, Arith, T1, Add, NRest).

% defunc_exp(Exp, Module, Arith, NewExp, AddGoal, RestGoal) :- NewExp is a
% expression without functions equivalent to Exp when adding goals
% AddGoal minus RestGoal.

% assumes is/2 is imported
defunc_exp(V,_Mod,_Arith, V, G, G) :- var(V), !.
defunc_exp(^(T0), Mod, Arith, T1, Add, Rest) :- !,
        functor(T0, F, A),
        functor(T1, F, A),
        defunc_args(A, T0, Mod, Arith, T1, Add, Rest).        
defunc_exp(Fun, Mod, true, V, Add, Rest) :-
        arith_exp(Fun), !,
        functor(Fun, F, A),
        functor(NFn, F, A),
        defunc_args(A, Fun, Mod, tempfalse, NFn, Add, (V is NFn, Rest)).
defunc_exp(lazy Fun, Mod, Arith, V, Add, Rest) :-
        is_function(Fun, Mod, Fn), !,
        functor(Fn, F, A),
        A1 is A+1,
        functor(NFn, F, A1),
        arg(A1, NFn, V),
        new_arith(Arith, NArith),
        defunc_args(A, Fn, Mod, NArith, NFn, Add, (freeze(V, NFn), Rest)).
defunc_exp(parallel Fun, Mod, Arith, V, Add, Rest) :-
        is_function(Fun, Mod, Fn), !,
        functor(Fn, F, A),
        A1 is A+1,
        functor(NFn, F, A1),
        arg(A1, NFn, V),
        new_arith(Arith, NArith),
        defunc_args(A, Fn, Mod, NArith, NFn, Add, (parallel(V, NFn), Rest)).
defunc_exp(Fun, Mod, Arith, V, Add, Rest) :-
        is_function(Fun, Mod, Fn), !,
        functor(Fn, F, A),
        A1 is A+1,
        functor(NFn, F, A1),
        arg(A1, NFn, V),
        new_arith(Arith, NArith),
        defunc_args(A, Fn, Mod, NArith, NFn, Add, (NFn, Rest)).
defunc_exp(T0, Mod, Arith, T1, Add, Rest) :-
        \+ (T0 = is(_,_)), % avoid infinite loop
        functor(T0, F, A),
        functor(T1, F, A),
        defunc_args(A, T0, Mod, Arith, T1, Add, Rest).

new_arith(false, false).
new_arith(tempfalse, true).
new_arith(true, true).

% defunc_goal(Goal, NewGoal) :- NewGoal is a goal equivalent to Goal but
% without functions.
% To be called as a goal translation
defunc_goal((U1 = U2), NewGoal, Mod) :-
        (V = U1, Fun = U2 ; V = U2, Fun = U1),
        var(V), nonvar(Fun),
        is_function(Fun, Mod, Fn), !,
        arith_flag(Mod, Arith_Flag),
        functor(Fn, F, A),
        A1 is A+1,
        functor(NFn, F, A1),
        arg(A1, NFn, V), 
        defunc_args(A, Fn, Mod, Arith_Flag, NFn, NewGoal, NFn).
defunc_goal(Goal, NewGoal, Mod) :-
        \+ (Goal = is(_,_)), % avoid infinite loop
        arith_flag(Mod, Arith_Flag),
        functor(Goal, F, A),
        functor(Goal1, F, A),
        NewGoal = (_,_),
        defunc_args(A, Goal, Mod, Arith_Flag, Goal1, NewGoal, Goal1).

arith_exp(-(_)).
arith_exp(+(_)).
arith_exp(--(_)).
arith_exp(++(_)).
arith_exp(+(_,_)).
arith_exp(-(_,_)).
arith_exp(*(_,_)).
arith_exp(/(_,_)).
arith_exp(//(_,_)).
arith_exp(rem(_,_)).
arith_exp(mod(_,_)).
arith_exp(#(_,_)).
arith_exp(/\(_,_)).
arith_exp(\/(_,_)).
arith_exp(\(_)).
arith_exp(<<(_,_)).
arith_exp(>>(_,_)).
arith_exp(integer(_)).
arith_exp(truncate(_)).
arith_exp(float(_)).
arith_exp(gcd(_,_)).
arith_exp(abs(_)).
arith_exp(sign(_)).
arith_exp(float_integer_part(_)).
arith_exp(float_fractional_part(_)).
arith_exp(floor(_)).
arith_exp(round(_)).
arith_exp(ceiling(_)).
arith_exp(**(_,_)).
arith_exp(exp(_)).
arith_exp(log(_)).
arith_exp(sqrt(_)).
arith_exp(sin(_)).
arith_exp(cos(_)).
arith_exp(atan(_)).

make_function(P, Mod) :-
        current_fact(function(P, Mod)), !.
make_function(P, Mod) :-
        asserta_fact(function(P, Mod)).

is_function(~(Fun),_Mod, Fun) :- !.
is_function(Fun, Mod, Fun) :- function(Fun, Mod).


concat_bodies(V, B, NB) :- var(V), !,
        del_last_true_(B, V, NB).
concat_bodies((G, Gs), B, (G, NB)) :- !,
        concat_bodies(Gs, B, NB).
concat_bodies(G, B, NB) :-
        del_last_true_(B, G, NB).

% del_last_true(Goal, NewGoal) :- Goal is a sequence whose last element is
% 'true', NewGoal the same sequence without this element, except if it is
% the only element 

del_last_true(true, true).
del_last_true((G, Gs), NG) :-
        del_last_true_(Gs, G, NG).

del_last_true_(true, G, G).
del_last_true_((G,Gs), G0, (G0,NG)) :-
        del_last_true_(Gs, G, NG).


% add_to_term(T, Add, NT) :- NT is a term equal to T but with one argument
% added at the end: Add.

add_to_term(T, Add, NT) :-
        functor(T, F, A),
        A1 is A+1,
        functor(NT, F, A1),
        arg(A1, NT, Add),
        copy_args(A, T, NT).
