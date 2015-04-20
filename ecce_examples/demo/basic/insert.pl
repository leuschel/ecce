
insert(X,nil,tree(nil,X,nil)).
insert(X,tree(L,X,R),tree(L,X,R)).
insert(X,tree(L,Y,R),tree(IL,Y,R)) :-
 	X<Y,insert(X,L,IL).
insert(X,tree(L,Y,R),tree(L,Y,IR)) :-
 	X>Y,insert(X,R,IR).


