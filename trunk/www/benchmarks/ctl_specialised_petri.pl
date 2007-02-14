
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
  
sat(X) :- 
  sat_eu__1(X,0,s(0),0,0).  /* initial marking: X number of processes */