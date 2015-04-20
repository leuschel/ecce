
:- module(cache_tr,[cache_exp/2],[]).

cache_exp((:- cache(F/A,File)),Cl):-
	atom(F),
	number(A),
	atom(File), !,
	functor(H,F,A),
	Cl=(H:-cache_rt:cache_call(File,H)).
cache_exp((:- cache(FA,F)),_):-
	warning(['Wrong declaration: :- ',''(cache(FA,F))]).
