
:- use_package([]).

:- use_module(library(lists)).

/*
Unless uncommenting M=lists, the operation is:

?- main(M).
[a,b]
M = - ? 

yes

*/
main(M):-
%	M=lists,
%	Call=
        M:append([a],[b],L),
%	call(Call),
	display(L).
