trace(s(X),[dec|T]) :- trace(X,T).
trace(0,[stop]).
trace(s(X),[inc|T]) :- trace(s(s(X)),T).

trace(f(X),[dec|T]) :- trace(X,T).
trace(f(X),[inc|T]) :- trace(f(f(X)),T).
trace(a,[inc,stop]).

sync_trace(T) :- trace(s(0),T), trace(f(a),T).

/* ecce with RUL:

sync_trace([inc,A|B]) :- 
    trace_conj__2(0,A,B,a).
sync_trace__1([inc,A|B]) :- 
    trace_conj__2(0,A,B,a).
trace_conj__2(A,dec,[B|C],D) :- 
    trace_conj__3(A,B,C,D).
trace_conj__2(A,inc,[B|C],D) :- 
    trace_conj__2(s(A),B,C,f(D)).
trace_conj__3(A,dec,[B|C],D) :- 
    trace_conj__4(A,B,C,D).
trace_conj__3(A,inc,[B|C],D) :- 
    trace_conj__2(A,B,C,D).
trace_conj__4(s(A),dec,[B|C],f(D)) :- 
    trace_conj__4(A,B,C,D).
trace_conj__4(s(A),inc,[B|C],f(D)) :- 
    trace_conj__2(A,B,C,D).
    
    After MSV:
    
sync_trace(([inc,A|B]) :- 
    fail.
    
    Top-Down RUL approx with SP:
    
sync_trace__ans(X1) :- t230(X1).
t230([X1|X2]) :- t231(X1),t232(X2).
t231(inc) :- true.
t231(dec) :- true.
t231(stop) :- true.
t232([X1|X2]) :- t233(X1),t232(X2).
t232([]) :- true.
t233(inc) :- true.
t233(dec) :- true.
t233(stop) :- true.

    */