/* file: map.pro */


map:map(P,[],[]).
map:map(P,[H|T],[PH|PT]) :-
	Call =.. [P,H,PH],
	call(map:Call),
	map:map(P,T,PT).


map:reduce(Func,Base,[],Base).
map:reduce(Func,Base,[H|T],Res) :-
	map:reduce(Func,Base,T,TRes),
	Call =.. [Func,H,TRes,Res],
	call(map:Call).


map:q(a,b).
map:q(b,c).
map:q(c,d).
map:q(d,e).

map:reduce_add(List,Res) :-
	map:reduce(add,0,List,Res).
map:add(X,Y,Z) :-
	Z is X + Y.


map:rev(L,R) :-
	map:rev(L,[],R).

map:rev([],L,L).
map:rev([H|T],A,R) :-
	map:rev(T,[H|A],R).
