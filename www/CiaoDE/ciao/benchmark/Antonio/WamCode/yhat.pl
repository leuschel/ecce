yHat(A, N, M, YHAT) :- M < N, !,
   UNTIL= M,
   yHATloop(A, 0, N, UNTIL, YHAT1),
   YHAT is -YHAT1.
