:- module(benchmarking,[
	supported_prolog/1,
	lips/1,
	bench/5,
	repeat/1,
	lots/3,
	eg_count/1],
	[assertions]).
%		       ],
%:- ['/home/clip/Systems/ciao/compatibility/sicstus3/ciaocompat.pl'].

:- use_module(library(prolog_sys)).
:- use_module(library(write)).

:- include(library(assertions)).
:- include(library(regtypes)).
:- include(library(isomodes)).

%:- true pred tan(in(X),go(Y)) :: num * num + (foreign,returns(Y)).


:- comment(title,"Benchmarking Support").

:- comment(module,"This library contains:

   @begin{itemize} 

   @item The standard @concept{naive reverse} benchmark traditionally
   used to measure @concept{LIPS} in Prolog systems.

   @item Some iteration facilities intended for measuring the
   execution speed of other Prolog programs.

   @end{itemize} 

   The library can be run on a number of Prolog systems, given by
   @pred{supported_prolog/1} below.").

% ---------------------------------------------------------------------------

:- regtype supported_prolog(S) # "@var{S} is a Prolog system supported by
   the @lib{benchmarking} library.".

supported_prolog(ciao).
supported_prolog(sicstus).

% ---------------------------------------------------------------------------
% The standard LIPS benchmark
% ---------------------------------------------------------------------------

:- comment(lips(System),"This benchmark gives the raw @index{speed of
   a Prolog system}.

   The measure of @index{logical inferences per second}
   (@concept{LIPS}) used here is taken to be procedure calls per
   second over an example with not very complex procedure calls. The
   example used is that of ``naive reversing'' a list, which is an
   expensive, and therefore stupid, way of reversing a list.  It does,
   however, produce a lot of procedure calls. (In theoretical terms,
   this algorithm is O(n^2) on the length of the list).

   The use of a single simple benchmark like this cannot, of course,
   be taken to signify a great deal. However, experience has shown
   that this benchmark does provide a good measure of basic Prolog
   speed and produces figures which match more complex benchmarks. The
   reason for this is that the basic operations performed here:
   procedure calls with a certain amount of data structure access and
   construction; are absolutely fundamental to Prolog execution. If
   these are done right, then more complex benchmarks tend to scale
   accordingly. This particular benchmark has thus been used as a good
   rule of thumb by Prolog implementors for over a decade and forms a
   part of the unwritten Prolog folklore. So - use this benchmark,
   with this in mind, as a quick, but useful, test of Prolog
   performance.

   In a complete evaluation of a Prolog system you should also be taking
   account speeds of asserting and compiling, tail recursion, memory
   utilisation, compactness of programs, storage management and garbage
   collection, debugging and editing facilities, program checking and help
   facilities, system provided predicates, interfaces to external
   capabilities, documentation and support, amongst other factors.").

:- pred lips(+System) # "Executes @pred{lots/3} for the LIPS benchmark
   (as calling @tt{lots(System,nrevdriver,nrevdriver_dummy)}, but
   reporting is specific to this benchmark).".

lips(System) :- 
	eg_count(Count),
	do_bench(System,Count,nrevdriver,nrevdriver_dummy,Time1,Time2),
	report(System,Count,Time1,Time2,lips),
	fail.
lips(_).


%% Drivers for the LIPS benchmark:

nrevdriver :-
	nrevdata(List),
	nrev(List,_).

nrevdriver_dummy :-
	nrevdata(List),
	nrev_dummy(List,_).

nrev_dummy(_,_).

:- comment(nrev/2,"This is the program executed by the @pred{lips}
   benchmark.  It is called @concept{naive reverse} because it is a
   very expensive way of reversing a list. Its advantage, for our
   purposes, is that it generates a lot of procedure calls. To reverse
   a thirty element list requires 496 Prolog procedure calls. The code is 
   as follows:  

   @begin{verbatim}
   @includedef{nrev/2}
   @end{verbatim}
").

:- pred nrev(L1,L2) # "@var{L2} is the list @var{L1} reversed.".

nrev([],[]).
nrev([X|Rest],Ans) :- 
	nrev(Rest,L), 
	append(L,[X],Ans).

:- comment(append/3,"The standard definition of append, used by the
   nrev benchmark. The code is as follows:

   @begin{verbatim}
   @includedef{append/3}
   @end{verbatim}
").

:- pred append(L1,L2,L3) # "@var{L1} appended to @var{L2} is @var{L3}.".

append([],L,L).
append([X|L1],L2,[X|L3]) :- 
	append(L1,L2,L3).

:- comment(nrevdata/1,"The @cindex{lips, data used} data used by the
   naive reverse benchmark. The code is as follows:

   @begin{verbatim}
   @includedef{nrevdata/1}
   @end{verbatim}
").

:- pred nrevdata(L)	# "@var{L} is a thirty element list.".

nrevdata([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,
      18,19,20,21,22,23,24,25,26,27,28,29,30]).

% ---------------------------------------------------------------------------
% Test (general purpose) harness 
% ---------------------------------------------------------------------------

:- comment(bench(System,Count,Bench,Dummy,TimePerIteration),"The
   purpose of @pred{bench/5} is to provide a @concept{test harness}
   for running @index{Prolog benchmarks}. The benchmark itself is run
   by calling predicate @var{Bench}, which should be a predicate of
   arity zero. @pred{bench/5} will use the exported predicate
   @pred{repeat/1} to iterate the right number of times, taking care
   of factoring out the overhead of setting the test up and of using
   @pred{repeat/1} to iterate. This is done by running dummy code to
   see how much time the extra operations take. This dummy code should
   be contained in a predicate called @var{Bench}@tt{_dummy}, which
   should also be of arity one, and which should contain all the setup
   operations that are not to be measured so that @pred{bench/5} can
   factor them out.  For large enough values of @var{Count},
   @var{TimePerIteration} should typically be constant.  See
   @pred{lots/3} for a (very simple) method for determining the right
   value for @var{Count}. @var{TimePerIteration} is the net time taken
   by each iteration running the benchmark (excluding the time taken
   by the setup and benchmarking overhead)

   As an example, these are the definitions of @pred{Bench} and the
   @concept{dummy predicate} for the classical @em{lips} @cindex{LIPS}
   benchmark (which measures the speed of @concept{naive reverse} --
   @pred{nrev/2}), which are included with this library:

   @begin{itemize}

   @item The driver for the benchmark itself:
   @begin{verbatim}
   @includedef{nrevdriver/0}
   @end{verbatim}

   @item The benchmark:
   @begin{verbatim}
   @includedef{nrev/2}

   @includedef{append/3}

   @includedef{nrevdata/1}
   @end{verbatim}

   @item The dummy driver:
   @begin{verbatim}
   @includedef{nrevdriver_dummy/0}

   @includedef{nrev_dummy/2}
   @end{verbatim}

   @end{itemize}

   ").


:- meta_predicate bench(?,?,:,:,?).

:- pred bench(+System,+Count,+Bench,+Dummy,-TimePerIteration) 
   :: supported_prolog * int * pred * pred * number

# "Run the benchmark @var{Bench} for @var{Count}
   iterations. @var{System} is the Prolog system on which we are
   running the benchmark. @var{Dummy} is the dummy driver used to
   compute the setup time and @index{benchmarking
   overhead}. @var{TimePerIteration} is the net time taken by each
   iteration running the benchmark.".

bench(System,Count,Bench,Dummy,TimePerIteration) :-
	do_bench(System,Count,Bench,Dummy,Time1,Time2),
	Time  is Time2-Time1, /* Time spent on benchmark itself */
	TimePerIteration is Time/Count.		

do_bench(System,Count,Bench,Dummy,Time1,Time2) :-
	get_cpu_time(System,T0),
	repeat_goal(Count,Dummy),
	get_cpu_time(System,T1),
	repeat_goal(Count,Bench),
	get_cpu_time(System,T2),
	Time1 is T1-T0,
	Time2 is T2-T1.

:- comment(lots/3,"This predicate is used to run a benchmark with
   increasing numbers of iterations.  The results produced for the
   different numbers of iterations should be similarm, except that
   there may be inaccuracies at low iteration numbers if the time that
   the benchmark takes to execute that number of time on your machine
   and system is too small to be very precise (because of the accuracy
   the operating system itself is capable of providing).  Good for
   determining how many iterations it is reasonable to measure for a
   given benchmark. If the time taken is too long or too short then
   you should adjust the @pred{eg_count/1} facts.").

:- meta_predicate lots(?,:,:).

:- pred lots(+System,+Bench,+Dummy) :: supported_prolog * pred * pred

# "Run the benchmark @var{Bench} with a variety of iteration
   counts.".

lots(System,Bench,Dummy) :-
	eg_count(Count),
	do_bench(System,Count,Bench,Dummy,Time1,Time2),
	report(System,Count,Time1,Time2,unittime),
	fail.
lots(_,_,_).

:- dynamic eg_count/1.

:- comment(eg_count/1,"").

:- pred eg_count(N) # "@var{N} is a number of iterations to be tried by 
   @pred{lots/3}. The default settings are as follows:

   @begin{verbatim}
   @includedef{eg_count/1}
   @end{verbatim}

   ".

%eg_count(10).
%eg_count(20).
%eg_count(50).
eg_count(100).
% eg_count(200).
% eg_count(500).
eg_count(1000).
% %% Added more for modern systems... MH
%% eg_count(2000).
%% eg_count(5000).
eg_count(10000).
%% eg_count(20000).
%% eg_count(50000).
%% eg_count(100000).
%% eg_count(200000).
%% eg_count(500000).
%% eg_count(1000000). % One million!

:- comment(doinclude,get_cpu_time/2).

:- pred get_cpu_time(+S,-T) :: supported_prolog * int

# "@var{T} is the @em{current} cpu time. This predicate is specific to
   the Prolog system because different systems provide this facility
   in different ways. See your Prolog manual in order to add support
   for your system.  Also check the code for
   @pred{calculate_lips/4}.".

%% get_cpu_time(quintus,T) :- statistics(runtime,[T,_]).
get_cpu_time(ciao,T)    :- statistics(walltime,[T,_]).
%% get_cpu_time(ciao,T)    :- statistics(runtime,[T,_]).

get_cpu_time(sicstus,T) :- statistics(runtime,[T,_]).
%% get_cpu_time(cprolog,T) :- T is cputime.

:- pred repeat_goal(+Count,+Goal) # "Iteration meta-predicate: executes
   @var{Goal} @var{Count} times.".

repeat_goal(Count,Goal) :-
	repeat(Count),
	Goal,
	fail.
repeat_goal(_Count,_Goal).

:- pred repeat(+Count) # "Iteration predicate: succeeds @var{Count} times.".

repeat(_N).
repeat(N) :- N > 1, N1 is N-1, repeat(N1).	


:- pred report(+System,+Count,+Time1,+Time2,+OutputType) 
	# "Report the results of the benchmark.".

report(System,Count,Time1,Time2,OutputType) :-
	Time is Time2-Time1,
        nl,
	(  OutputType == lips
	-> calculate_lips(System,Count,Time,Lips,Units),
	   write(Lips), write(' lips for ')
	;  calculate_iteration_time(System,Count,Time,UnitTime,Units),
	   write(UnitTime), write(' '), write(Units), 
	   write('/iteration for ')
	),
	write(Count),
	write(' iterations taking '), write(Time),
	write(' '), write(Units), write(' ('),
	write(Time2-Time1), write(') total.'),
	nl.

:- comment(doinclude,calculate_lips/5).

:- pred calculate_lips(+System,+Count,+Time,-Lips,-Units) 

# "Doing @var{Count} interations in @var{Time} implies @var{Lips} lips
   assuming that time is given in @var{Units}.

   This calculates the logical inferences per second (lips) figure.
   Remember that it takes 496 procedure calls to naive reverse a
   thirty element list once. Lips, under this benchmark, thus means
   ``Prolog procedure calls per second, where the procedure calls are
   not too complex (i.e. those for nrev and append).''

   This version of the code assumes that the times (T0.. etc) are
   integers giving the time in milliseconds. This is true for Quintus
   Prolog.  Your Prolog system may use some other representation.  If
   so, you will need to adjust the Lips calculation.  There is a
   C-Prolog version below for the case where times are floating point
   numbers giving the time in seconds.".

calculate_lips(_System,_Count,Time,Lips,'mS') :- 
	% Time can be 0 for small values of Count!
	Time is 0, !, Lips = '(undefined)'.
calculate_lips(ciao,Count,Time,Lips,'mS') :- 
	!, Lips  is integer((496*float(Count)*1000)/Time).
calculate_lips(sicstus,Count,Time,Lips,'mS') :- 
	!, Lips  is integer((496*float(Count)*1000)/Time).
calculate_lips(cprolog,Count,Time,Lips,'S') :- 
	!, Lips  is (496*Count)/Time.

:- pred calculate_iteration_time(+System,+Count,+Time,-UnitTime,-Units) 

# "Doing @var{Count} interations in @var{Time} implies @var{UnitTime}
   per iterations, with time given in @var{Units}. Used for benchmarks
   other than @index{naive reverse}.".

calculate_iteration_time(_System,_Count,Time,UnitTime,'mS') :- 
	% Time can be 0 for small values of Count!
	Time is 0, !, UnitTime = '(undefined)'.
calculate_iteration_time(ciao,Count,Time,UnitTime,'mS') :- 
	!, UnitTime  is Time/Count.
calculate_iteration_time(sicstus,Count,Time,UnitTime,'mS') :- 
	!, UnitTime  is Time/float(Count).
calculate_iteration_time(cprolog,Count,Time,UnitTime,'S') :- 
	!, UnitTime  is Time/Count.
