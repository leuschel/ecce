:- module(testbench,_).
%%[doit/0]).

:- use_module(benchmarking).

doit :- 
	lots(ciao,testdriver,testdriver_dummy).

testdriver :-
	testdata(List),
	test(List,_).

testdriver_dummy :-
	testdata(List),
	test_dummy(List,_).

test_dummy(_,_).

test([],[]).
test([X|Rest],Ans) :- 
	test(Rest,L), 
	testappend(L,[X],Ans).

testappend([],L,L).
testappend([X|L1],L2,[X|L3]) :- 
	testappend(L1,L2,L3).

testdata([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,
      18,19,20,21,22,23,24,25,26,27,28,29,30]).
