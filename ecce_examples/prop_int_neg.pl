int(const(true)). 
int(const(false)) :- fail. 
int(and(X,Y)) :- int(X), int(Y). 
%int(or(X,Y)) :- int(X) ; nint(X),int(Y). 
int(or(X,_)) :- int(X). 
int(or(X,Y)) :- nint(X),int(Y). 
int(not(X)) :- nint(X).
int(equiv(A,B)) :- 
   int(or(and(A,B),and(not(A),not(B)))).

nint(const(false)). 
nint(const(true)) :- fail. 
%nint(and(X,Y)) :- nint(X) ; int(X),nint(Y).
nint(and(X,_)) :- nint(X).
nint(and(X,Y)) :- int(X),nint(Y).
nint(or(X,Y)) :- nint(X),nint(Y). 
nint(not(X)) :- int(X). 
nint(equiv(A,B)) :- 
   not_int(or(and(A,B),and(not(A),not(B)))).



% T1 = const(X), T2 = or(T1,T1), T3 = and(T2,T2), T4 = and(T3,T3), int(T4)


% Smullyan Frage 3:
% “Mindestens einer von uns beiden ist ein Schurke”
test3(A,B) :- int(equiv(const(A),or(not(const(A)),not(const(B))))).