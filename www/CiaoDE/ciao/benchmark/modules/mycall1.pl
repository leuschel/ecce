
c(N) :- q(X), mycall(utilities:length,[X,N]).

q([1,2,3,4]).

mycall(M:X,Args) :- Call=..[X|Args], M:Call.
% this clause should give type error in =../2 but does not (in ciao)
mycall(X,Args) :- Call=..[X|Args], Call.
