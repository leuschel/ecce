    
delete(X,[X|T],T).
delete(X,[H|T],[H|DT]) :- X\=H, delete(X,T,DT).

member(X,L) :- delete(X,L,_).  /* use RAF for member(X,L) */
