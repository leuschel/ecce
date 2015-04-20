/* ======================================================================
   This benchmark gives the raw speed of a Prolog system.

   The measure of logical inferences per second (Lips) used here is taken to
   be procedure calls per second over an example with not very complex
   procedure calls. The example used is that of "naive reversing" a list,
   which is an expensive, and therefore stupid, way of reversing a list.  It
   does, however, produce a lot of procedure calls. (In theoretical terms,
   this algorithm is O(n^2) on the length of the list).

   The use of a single simple benchmark like this cannot, of course, be
   taken to signify a great deal. However, experience has shown that this
   benchmark does provide a very good measure of basic Prolog speed and
   produces figures which match more complex benchmarks. The reason for
   this is that the basic operations performed here: procedure calls with a
   certain amount of data structure access and construction; are absolutely
   fundamental to Prolog execution. If these are done right, then more
   complex benchmarks tend to scale accordingly. This particular benchmark
   has thus been used as a good rule of thumb by Prolog implementors for
   over a decade and forms a part of the unwritten Prolog folklore. So -
   use this benchmark, with this in mind, as a quick, but extremely useful,
   test of Prolog performance.

   In a complete evaluation of a Prolog system you should also be taking
   account speeds of asserting and compiling, tail recursion, memory
   utilisation, compactness of programs, storage management and garbage
   collection, debugging and editing facilities, program checking and help
   facilities, system provided predicates, interfaces to external
   capabilities, documentation and support, amongst other factors.

   ====================================================================== */


%% For CIAO - MH
main :- lots.

:- pred dobench(Count) # "nrev a 30 element list @var{Count} times.".

dobench(Count) :-
	data(List),
	repeat(Count),
	nrev(List,_),
	fail.
dobench(_).

:- pred dodummy(Count) # "Perform the overhead operations @var{Count} times.".

dodummy(Count) :-
	data(List),
	repeat(Count),
	dummy(List,_),
	fail.
dodummy(_).

dummy(_,_).

/* ----------------------------------------------------------------------
   ---------------------------------------------------------------------- */

nrev([],[]).
nrev([X|Rest],Ans) :- nrev(Rest,L), append(L,[X],Ans).

append([],L,L).
append([X|L1],L2,[X|L3]) :- append(L1,L2,L3).


data([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,
			   21,22,23,24,25,26,27,28,29,30]).
