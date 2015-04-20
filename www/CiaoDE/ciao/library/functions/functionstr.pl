:- module(
	functionstr,
	[defunc/3, defunc_goal/3],
	['functions/ops', assertions]
	 ).

:- use_module(library(terms), [copy_args/3]).

:- data function/3.
:- data ignore_arith/1.
:- data defined_functions/1.
:- data fun_return/4.

% defunc(FuncItem, PredItem, Module) :- PredItem is a clause, query or
% command equivalent to FuncItem but without functions.
% To be called as a sentence translation

defunc(0, _, Mod) :- !,
        retractall_fact(function(_,Mod,_)),
        retractall_fact(ignore_arith(Mod)),
        retractall_fact(defined_functions(Mod)),
        retractall_fact(fun_return(_,_,Mod,_)).
defunc(end_of_file, end_of_file, _) :- !.
defunc((?- _), _, _) :- !, fail.
defunc((:- function(Spec)), _, Mod) :- !,
        ( Spec = QM:F/A, functor(P, F, A) ->
            make_function(P, Mod, QM)
        ; Spec = F/A, functor(P, F, A) ->
            make_function(P, Mod, (-))
        ; Spec = arith(true) ->
            retractall_fact(ignore_arith(Mod))
        ; Spec = arith(false) ->
            asserta_fact(ignore_arith(Mod))
        ; Spec = defined(true) ->
            asserta_fact(defined_functions(Mod))
        ; Spec = defined(false) ->
            retractall_fact(defined_functions(Mod))
        ; function_output_arg(Spec, Fun, A, QM) ->
            asserta_fact(fun_return(Fun, A, Mod, QM)),
            make_function(Fun, Mod, QM)
        ; error(['Invalid function specification: ',Spec])
        ).
defunc((:- fun_return(FSpec)), _, Mod) :- !,
        ( function_output_arg(FSpec, Fun, A, QM) ->
            asserta_fact(fun_return(Fun, A, Mod, QM))
        ;
            error(['Invalid fun_return specification: ',FSpec])
        ).
defunc((:- lazy(function(Spec))), (:- lazy(NewSpec)), Mod) :-
	!,
	Spec = F/A,
	NewA is A + 1,
	NewSpec = F/NewA,
	defunc((:- function(Spec)), _, Mod).
defunc((:- _), _, _) :- !, fail.
defunc((FuncHead := FuncValOpts), Clauses, Mod) :-
        nonvar(FuncValOpts),
        FuncValOpts = (FuncVal1 | FuncValR),
        !,
        Clauses = [Clause1 | ClauseR],
        defunc((FuncHead := FuncVal1), Clause1, Mod),
        defunc((FuncHead := FuncValR), ClauseR, Mod).
defunc((FuncHead := CondFuncVal), (Head :- Body), Mod) :-
        nonvar(CondFuncVal),
        CondFuncVal = (Cond ? FuncValue),
        !,
        arith_flag(Mod, Arith_Flag),
        defunc_head(FuncHead, Mod, Arith_Flag, NewFuncHead, Body, RestBody1),
        add_return_arg(NewFuncHead, F, A, Val, Mod, Head),
        make_tmp_function(F, A, Mod, Ref),
        defunc_exp(FuncValue, Mod, Arith_Flag, NewFuncValue, RestBody2, true),
        delt_tmp_function(Ref),
        concat_bodies(Cond, (!, Val=NewFuncValue, RestBody2), RestBody1).
defunc((FuncHead := FuncValue), (Head :- Body), Mod) :- !,
        arith_flag(Mod, Arith_Flag),
        defunc_head(FuncHead, Mod, Arith_Flag, NewFuncHead, AddBody, RestBody),
        add_return_arg(NewFuncHead, F, A, NewFuncValue, Mod, Head),
        make_tmp_function(F, A, Mod, Ref),
        defunc_exp(FuncValue, Mod, Arith_Flag, NewFuncValue, RestBody, true),
        delt_tmp_function(Ref),
        del_last_true(AddBody, Body).
defunc((FuncHead := FuncValue :- FuncBody), (Head :- Body), Mod) :- !,
        arith_flag(Mod, Arith_Flag),
        defunc_head(FuncHead, Mod, Arith_Flag, NewFuncHead, AddBody, FuncBody),
        add_return_arg(NewFuncHead, F, A, NewFuncValue, Mod, Head),
        make_tmp_function(F, A, Mod, Ref),
        defunc_exp(FuncValue, Mod, Arith_Flag, NewFuncValue, LastBody, true),
        delt_tmp_function(Ref),
        concat_bodies(AddBody, LastBody, Body).
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

% defunc_list(List, Module, Arith_Flag, NewList, AddGoal, RestGoal) :-

defunc_list([], _, _, [], X, X) :- !.
defunc_list([T|Ts], Mod, Arith, [NT|NTs], Add, Rest) :-
        defunc_exp(T, Mod, Arith, NT, Add, Rest0),
        defunc_list(Ts, Mod, Arith, NTs, Rest0, Rest).

% defunc_exp(Exp, Module, Arith, NewExp, AddGoal, RestGoal) :- NewExp is
% a expression without functions equivalent to Exp when adding goals
% AddGoal minus RestGoal.

% assumes is/2 is imported
defunc_exp(V,_Mod,_Arith, V, G, G) :- var(V), !.
defunc_exp(^T0, Mod, Arith, T1, Add, Rest) :- !, % Only works in heads
        functor(T0, F, A),
        functor(T1, F, A),
        defunc_args(A, T0, Mod, Arith, T1, Add, Rest).        
defunc_exp(^^(T),_Mod,_Arith,^^(T), G, G) :- !.
defunc_exp(Fun, Mod, true, V, Add, Rest) :-
        arith_exp(Fun), !,
        functor(Fun, F, A),
        functor(NFn, F, A),
        defunc_args(A, Fun, Mod, tempfalse, NFn, Add, (V is NFn, Rest)).
defunc_exp(Fun, Mod, Arith, V, Add, Rest) :-
        is_function(Fun, Mod, Fn, QM, Ret_Arg), !,
        new_arith(Arith, NArith),
        Fn =.. [F|FArgs],
        defunc_list(FArgs, Mod, NArith, NFArgs, Add, (QNFn, Rest)),
        add_nth(Ret_Arg, NFArgs, V, NFArgsV),
        NFn =.. [F|NFArgsV],
        add_qualification(QM, NFn, QNFn).
defunc_exp(Opts, Mod, Arith, V, Add, Rest) :- Opts = (_|_), !,
        Add = (Assigns, Rest),
        defunc_opts(Opts, Mod, Arith, V, Assigns).
defunc_exp((Cond ? Val), Mod, Arith, V, Add, Rest) :- !,
        Add = ((Cond -> Assign), Rest),
        defunc_exp(Val, Mod, Arith, NVal, Assign, (V = NVal)).
defunc_exp(T0, Mod, Arith, T1, Add, Rest) :-
        ( T0 = is(_,_) ->
            NArith = tempfalse % avoid infinite loop
        ;
            NArith = Arith
        ),
        functor(T0, F, A),
        functor(T1, F, A),
        defunc_args(A, T0, Mod, NArith, T1, Add, Rest).

add_qualification(Q, P, P) :- Q == (-), !.
add_qualification(QM, P, QM:P).

new_arith(false, false).
new_arith(tempfalse, true).
new_arith(true, true).

defunc_opts(A,_Mod,_Arith, V, (V = A)) :- var(A), !.
defunc_opts((A|B), Mod, Arith, V, (A_As ; Assigns)) :- !,
        defunc_opt(A, Mod, Arith, V, A_As),
        defunc_opts(B, Mod, Arith, V, Assigns).
defunc_opts(A, Mod, Arith, V, A_As) :-
        defunc_opt(A, Mod, Arith, V, A_As).

defunc_opt(Val,_Mod,_Arith, V, (V = Val)) :- var(Val), !.
defunc_opt((Cond ? Val), Mod, Arith, V, (Cond -> Assign)) :- !,
        defunc_exp(Val, Mod, Arith, NVal, Assign, (V = NVal)).
defunc_opt(Val, Mod, Arith, V, Assign) :-
        defunc_exp(Val, Mod, Arith, NVal, Assign, (V = NVal)).

% defunc_goal(Goal, NewGoal, Module) :-
%   NewGoal is a goal equivalent to Goal (in Module) but without functions.
% To be called as a goal translation
defunc_goal(^^(G), G,_Mod) :- !.
defunc_goal((U1 = U2), NewGoal, Mod) :-
        (V = U1, Fun = U2 ; V = U2, Fun = U1),
        var(V), nonvar(Fun),
        is_function(Fun, Mod, Fn, QM, Ret_Arg), !,
        arith_flag(Mod, Arith_Flag),
        Fn =.. [F|FArgs],
        defunc_list(FArgs, Mod, Arith_Flag, NFArgs, NewGoal, QNFn),
        add_nth(Ret_Arg, NFArgs, V, NFArgsV),
        NFn =.. [F|NFArgsV],
        add_qualification(QM, NFn, QNFn).
defunc_goal(QM:Goal, NewGoal, Mod) :- !,
        arith_flag(Mod, Arith_Flag),
        functor(Goal, F, A),
        functor(Goal1, F, A),
        defunc_args(A, Goal, Mod, Arith_Flag, Goal1, NewGoal, QM:Goal1),
        NewGoal \== QM:Goal.
defunc_goal(Goal, NewGoal, Mod) :-
        ( Goal = is(_,_) ->
            Arith_Flag = tempfalse % avoid infinite loop
        ; 
            arith_flag(Mod, Arith_Flag)
        ),
        functor(Goal, F, A),
        functor(Goal1, F, A),
        defunc_args(A, Goal, Mod, Arith_Flag, Goal1, NewGoal, Goal1),
        NewGoal \== Goal.

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

make_function(P, Mod, QM) :-
        current_fact(function(P, Mod, QM)), !.
make_function(P, Mod, QM) :-
        asserta_fact(function(P, Mod, QM)).

make_tmp_function(F, A, Mod, Ref) :-
        defined_functions(Mod), !,
        functor(P, F, A),
        ( current_fact(function(P, Mod, (-))) ->
            Ref = []
        ;
            asserta_fact(function(P, Mod, (-)), Ref)
        ).
make_tmp_function(_F, _A, _Mod, []).

delt_tmp_function([]) :- !.
delt_tmp_function(Ref) :- erase(Ref).

function_output_arg(QM:FSpec, Fun, Arg, QM) :-
        nonvar(QM), !,
        function_output_arg(FSpec, Fun, Arg, (-)).
function_output_arg(FSpec, Fun, Arg, (-)) :-
        functor(FSpec, F, A),
        has_tilde(A, FSpec, Arg),
        A1 is A-1,
        functor(Fun, F, A1).

all_vars(0, _) :- !.
all_vars(A, F) :-
        arg(A, F, F_A),
        var(F_A),
        A1 is A-1,
        all_vars(A1, F).

is_function(~V,_Mod, call(V), (-), 2) :- var(V), !. % Apply?
is_function(~(QM:Fun), Mod, Fn, QM, Ret_Arg) :- !,
        fun_return_extended(Fun, Ret_Arg, Mod, QM, Fn).
is_function(~(Fun), Mod, Fn, (-), Ret_Arg) :- !,
        fun_return_extended(Fun, Ret_Arg, Mod, (-), Fn).
is_function(QM:Fun, Mod, Fun, QM, Ret_Arg) :- !,
        function(Fun, Mod, QM),
        fun_return_of(Fun, Ret_Arg, Mod, QM).
is_function(Fun, Mod, Fun, (-), Ret_Arg) :-
        function(Fun, Mod, (-)),
        fun_return_of(Fun, Ret_Arg, Mod, (-)).

fun_return_extended(Fun, Arg,_Mod,_QM, Fn) :-
        functor(Fun, F, A),
        has_tilde(A, Fun ,Arg), !, % Quick check
        A1 is A-1,
        functor(Fn, F, A1),
        take_out_arg(A, Arg, Fun, Fn).
fun_return_extended(Fun, Arg, Mod, QM, Fun) :-
        fun_return_of(Fun, Arg, Mod, QM).

has_tilde(N, F, Arg) :-
        arg(N, F, F_N),
        N1 is N-1,
        ( F_N == (~) ->
            Arg = N,
            not_tilde(N1, F)
        ; has_tilde(N1, F, Arg)
        ).

not_tilde(0, _) :- !.
not_tilde(A, F) :-
        arg(A, F, F_A),
        F_A \== (~), !,
        A1 is A-1,
        not_tilde(A1, F).
not_tilde(_, F) :-
        error(['More than one "~" marking function return argument in ',F]).

take_out_arg(A, A, Fun, Fn) :- !,
        A1 is A-1,
        copy_args(A1, Fun, Fn).
take_out_arg(A, Arg, Fun, Fn) :-
        A1 is A-1,
        arg(A, Fun, X),
        arg(A1, Fn, X),
        take_out_arg(A1, Arg, Fun, Fn).

fun_return_of(Fun, Arg, Mod, QM) :-
        fun_return(Fun, Arg, Mod, QM), !.
fun_return_of(Fun, Arg, _, _) :-
        functor(Fun, _, A),
        Arg is A+1.

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

add_return_arg(Func, F, A, Add, Mod, Pred) :-
        functor(Func, F, A),
        NA is A+1,
        functor(Pred, F, NA),
        fun_return_of(Func, Arg, Mod, (-)),
        arg(Arg, Pred, Add),
        PrevArg is Arg-1,
        copy_args(PrevArg, Func, Pred),
        copy_last_args(Arg, Func, Pred).

copy_last_args(A, T, NT) :-
        arg(A, T, X), !,
        A1 is A+1,
        arg(A1, NT, X),
        copy_last_args(A1, T, NT).
copy_last_args(_, _, _).

% PRE: integer(A), A > 0

add_nth(1, L, X, [X|L]).
add_nth(A, [E|Es], X, [E|NEs]) :-
        A1 is A-1,
        add_nth(A1, Es, X, NEs).
