:- module(_,[expand/2],[]).
% Expansion for clp(R).

:- use_module(library('clpr/eval_r')).

expand(arith_zero(Exp), G) :-
  arith_eps(Eps), NEps is -Eps,
  ( var(Exp) ->
      G = (Exp =< Eps, Exp >= NEps)
  ;
      G = (Res is Exp, Res =< Eps, Res >= NEps)
  ).

expand(arith_eval(Exp,V), G) :-
        G = (V is Exp).
expand(arith_eval(A<B), G) :-
        arith_eps(Eps), NEps is -Eps,
        G = (A-B < NEps).
expand(arith_eval(A=<B), G) :-
        arith_eps(Eps),
        G = (A-B < Eps).
expand(arith_eval(B>A), G) :-
        arith_eps(Eps), NEps is -Eps,
        G = (A-B < NEps).
expand(arith_eval(B>=A), G) :-
        arith_eps(Eps),
        G = (A-B < Eps).
