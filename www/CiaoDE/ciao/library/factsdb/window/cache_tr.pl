
:- module(cache_tr,[cache_exp/2],[]).

cache_exp((:- cache(F/A,File,N)),[C1,C2]):-
	atom(F),
	number(A),
	atom(File),
	number(N),
	N > 0, !,
	functor(H,F,A),
	H=..[F|Args],
	C1=(H:-cache_rt:cache_call(F,A,Args)),
	C2=('$cache$cached_goal'(F,A,File,N)).
cache_exp((:- cache(F/A,File)),[C1,C2]):-
	atom(F),
	number(A),
	atom(File), !,
	functor(H,F,A),
	C1=(H:-cache_rt:cache_call(F,A,H)),
	C2=('$cache$cached_goal'(F,A,File,0)).
cache_exp((:- cache(FA,F,N)),_):-
	warning(['Wrong declaration: :- ',''(cache(FA,F,N))]).
cache_exp((:- cache(FA,F)),_):-
	warning(['Wrong declaration: :- ',''(cache(FA,F))]).
