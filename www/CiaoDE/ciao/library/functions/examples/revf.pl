:- module(_, [nrev/2], [functions]).

nrev( [] )    := [].
nrev( [H|L] ) := ~conc( ~nrev(L),[H] ).

conc( [],    L ) := L.
conc( [H|L], K ) := [ H | ~conc(L,K) ]. 
