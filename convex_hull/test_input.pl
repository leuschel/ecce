

applast(L,X,Last) :- append(L,[X],LX),last(Last,LX).


last(X,[X]).
last(X,[H|T]) :- last(X,T).

append([],L,L).
append([H|L1],L2,[H|L3]) :- append(L1,L2,L3).


p(a).
p(b).

q(X) :- p(X).

int(0).
int(s(X)) :- int(X).

p(0,1).
p(X,Y) :- X1 is X-1, p(X1,Y).

double(0,0).
double(s(s(X)),s(Y)) :- double(X,Y).

same_len([],[]).
same_len([_|T],[_|T2]) :- same_len(T,T2).



mergesort([],[]).
mergesort([X],[X]).
mergesort([X,Y|Xs],Ys) :- split([X,Y|Xs],X1s,X2s),
              mergesort(X1s,Y1s), mergesort(X2s,Y2s), merge(Y1s,Y2s,Ys).

split([],[],[]).
split([X|Xs],[X|Ys],Zs) :- split(Xs,Zs,Ys).

merge([],Xs,Xs).
merge(Xs,[],Xs).
merge([X|Xs],[Y|Ys],[X|Zs]) :- X=<Y, merge(Xs,[Y|Ys],Zs).
merge([X|Xs],[Y|Ys],[Y|Zs]) :- X>Y, merge([X|Xs],Ys,Zs).



/* CTL Model Checker; specialised using Logen
   for a simple Petri Net with a Semaphore and for
   the formula EF(unsafe) where unsafe is that two
   or more processes are in the critical section = place
   3 has at least two tokens */
   
sat_eu__1(B,C,s(s(D)),E,F).  /* we have found an unsafe marking */
sat_eu__1(s(G),s(H),I,J,K) :- 
  sat_eu__1(G,H,s(I),J,K).
sat_eu__1(L,M,s(N),O,P) :- 
  sat_eu__1(L,s(M),N,s(O),P).
sat_eu__1(Q,R,S,s(T),U) :- 
  sat_eu__1(s(Q),R,S,T,s(U)).
start(X,0,s(0),0,0).

sat(X) :- 
  sat_eu__1(X,0,s(0),0,0).  /* initial marking: X number of processes */            
  
  
  
  
  
  
  
  
  
  
  
ins(X,[],[X]).
ins(X,[X|T],[X|T]).
ins(X,[H|T],[H|IT]) :- X\=H, ins(X,T,IT).


test(A,B,[],R) :- ins(A,[],R).                    