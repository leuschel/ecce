match:match(Pat,T) :- match:match1(Pat,T,Pat,T).

match:match1([],Ts,P,T).
match:match1([A|Ps],[B|Ts],P,[X|T]) :-
	A\==B,
	match:match1(P,T,P,T).
match:match1([A|Ps],[A|Ts],P,T) :-
	match:match1(Ps,Ts,P,T).



