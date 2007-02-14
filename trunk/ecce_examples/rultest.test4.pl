test4([a,b,a|A]) :- 
    trans_conj__3(a,A), 
    trace__5(agent(v2),prefix(b,stop),A).
test4__1([a,b,a|A]) :- 
    trans_conj__3(a,A), 
    trace__5(agent(v2),prefix(b,stop),A).
trans_conj__3(b,[]).
trans_conj__3(a,[b,A|B]) :- 
    trans_conj__3(A,B).
trace__5(A,B,[]) :- 
    stop__6(A,B).
trace__5(A,B,[C|D]) :- 
    trans_conj__7(A,B,C,D).
stop__6(A,B) :- 
    stop__9(A,B).
stop__6(A,B).
trans_conj__7(A,B,C,D) :- 
    trans__8(A,C,E), 
    trace__5(E,B,D).
trans_conj__7(A,prefix(b,stop),b,B) :- 
    trace__5(A,stop,B).
trans__8(prefix(A,B),A,B).
trans__8(interleave(A,B),C,interleave(D,B)) :- 
    trans__8(A,C,D).
trans__8(interleave(A,B),C,interleave(A,D)) :- 
    trans__8(B,C,D).
trans__8(agent(v2),a,interleave(agent(v2),prefix(b,stop))).
stop__9(A,B) :- 
    stop__10(A).
stop__9(A,stop).
stop__10(stop).
stop__10(interleave(A,B)) :- 
    stop__10(A).
stop__10(interleave(A,B)) :- 
    stop__10(B).