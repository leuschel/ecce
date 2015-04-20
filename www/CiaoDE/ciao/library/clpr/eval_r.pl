:- module(_, [arith_eps/1, arith_zero/1, arith_eval/2, arith_eval/1], []).

% low level arithmetic for clp(r,q,z)
%
% arith_zero(+Exp)
% arith_eval(+Exp <rel> +Exp)
% arith_eval(+Exp, -Res)

arith_eps(1.0e-10).

% EPS = 1e-10 is from Monash machine.c

%                -eps   0  +eps
%   ---------------[----|----]----------------
%            < 0                  > 0
%      <-----------]         [----------->
%           =< 0
%      <---------------------]
%                                 >= 0
%                  [--------------------->

arith_eval(Exp, Res) :- Res is Exp.

arith_eval(A < B) :-  % A < B.
  A-B < -1.0e-10.
arith_eval(A =< B) :-
  A-B <  1.0e-10.
arith_eval(A > B) :-
  A-B >  1.0e-10.
arith_eval(A >= B) :-
  A-B > -1.0e-10.
arith_eval(A = B) :-
  D is A-B,
  D =<  1.0e-10,
  D >= -1.0e-10.

arith_zero(Exp) :-
  Res is Exp,		% compute Res once only
  Res =<  1.0e-10,
  Res >= -1.0e-10.
