:- module(foo,[sqr/2],[foreign_interface]).

:- true pred sqr(in(X), go(Y)) :: int * int + (foreign,returns(Y)).
:- foreign_inline(sqr/2,
"int sqr(int x) {
  return x*x;
}
").
