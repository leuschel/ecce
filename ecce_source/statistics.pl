
/* --------------------------------------------- */
/* (C) COPYRIGHT MICHAEL LEUSCHEL 1995,1996,1997 */
/* --------------------------------------------- */

/* TO GET THE LATEST VERSION OF THE ECCE SYSTEM GO TO:
	http://www.cs.kuleuven.ac.be/~michael */
/* BUG REPORTS, QUESTIONS AND COMMENTS CAN BE SENT TO:
	michael@cs.kuleuven.ac.be */

/* file: statistics.pro */


:- dynamic last_bench_path/1.
last_bench_path('~/ecce/dppd/').

set_last_bench_path(New) :-
	retract(last_bench_path(_X)),fail.
set_last_bench_path(New) :-
	assert(last_bench_path(New)).

get_file_statistics(File,AU,UU,AB) :-
	please(tw,off),
	table('C',collect), /* perform garbage collection */
	please(tw,on),
	statistics('C',AllocUnits,UsedUnits,AllocBytes),
	reconsult(File),
	statistics('C',AllocUnits2,UsedUnits2,AllocBytes2),
	AU is AllocUnits2 - AllocUnits,
	UU is UsedUnits2 - UsedUnits,
	AB is AllocBytes2 - AllocBytes,
	myretractfile(File).
	
myretractfile(File) :-
	file_predicates(File,PredList),
	stat_member(Pred,PredList),
	retractfile(Pred,File),
	fail.
myretractfile(_).

stat_menu :-
	print('file to inspect (f for file set, q to quit, s for set, x for exit)'),nl,
	print(' ==>'),
	read(File),
	File \== q,
	((File = x) -> stop ; true),
	((File = s)
	 -> (stat_benchmark_set)
	 ;  ((File = f) -> (stat_file_set)
	 ;  (
	     get_file_statistics(File,AU,UU,AB),
	     print('Allocated Units = '), print(AU),nl,
	     print('Used Units      = '), print(UU),nl,
	     print('Allocated Bytes = '), print(AB),nl,nl
	   ))
	),
	stat_menu.


stat_benchmark_set :-
	seen,told,
	print('Benchmark Set File must contain:'),nl,
	print('  benchmark_set/1'),
	nl,
	
	print('Path to Benchmark Set File (BENCH_DIR):'),
	expand_path('$BENCH_DIR/',BenchPath),print(BenchPath),nl,
	print('Benchmark Set File (b for benchmarkset.bs) =>'),

	read(ReadBMSFile),
	((ReadBMSFile = b)
	 -> (BMSFile = 'benchmarkset.bs')
	 ;  (BMSFile = ReadBMSFile)
	),

	string_concatenate(BenchPath,BMSFile,FullBMSFile),
	see(FullBMSFile),
	(read(benchmark_set(Files)) -> true ; (Files = [])),
	seen,
	

	print('Program Files will be read from ECCE_ORIG_DIR:'),nl,
	expand_path('$ECCE_ORIG_DIR/',BenchOrigPath),print(BenchOrigPath),nl,

	stat_member(BMFile,Files),
	string_concatenate(BenchOrigPath,BMFile,FullBMFile),
	string_concatenate(FullBMFile,'.pro',FullEcceFile),
	get_file_statistics(FullEcceFile,AU,UU,AB),
	print('File            ='), print(FullEcceFile),nl,
	print('Used Units      = '), print(UU),nl,nl,
	fail.
stat_benchmark_set.

stat_file_set :-
	seen,told,
	print('File Set File must contain:'),nl,
	print('  benchmark_set/1'),
	nl,
	last_bench_path(LastPath),
	print('Path to Benchmark File'),nl,
	print(' (l = '),print(LastPath),
	print(') =>'),
	read(NewPath),
	((NewPath = l)
	 -> (BenchPath = LastPath)
	 ;  (BenchPath = NewPath, set_last_bench_path(NewPath))
	),
	print('Benchmark Set File =>'),
	read(BMSFile),
	string_concatenate(BenchPath,BMSFile,FullBMSFile),
	see(FullBMSFile),
	(read(benchmark_set(Files)) -> true ; (Files = [])),
	seen,
	stat_member(BMFile,Files),
	string_concatenate(BenchPath,BMFile,FullBMFile),
	get_file_statistics(FullBMFile,AU,UU,AB),
	print('File            ='), print(FullBMFile),nl,
	print('Used Units      = '), print(UU),nl,nl,
	fail.
stat_file_set.


stat_member(X,[X|_T]).
stat_member(X,[_Y|T]) :- stat_member(X,T).

stat_append([],L,L).
stat_append([H|X],Y,[H|Z]) :- stat_append(X,Y,Z).

/* generate a new symbol with the given prefix */
/* code from the "Art of Prolog" */
gensym(Prefix,V) :-
	var(V),
	atom(Prefix),
	oldvalue(Prefix,N),
	N1 is N + 1,
	set_flag(gensym(Prefix),N1),
	name(PE_Sep,"__"),
	string_concatenate(Prefix,PE_Sep,PreSep),
	string_concatenate(PreSep,N1,V).

gennum(Nr) :-
	oldvalue(num__num,Nr),
	N1 is Nr + 1,
	set_flag(gensym(num__num),N1).

reset_gennum(Nr) :-
	set_flag(gensym(num__num),Nr).

oldvalue(Prefix,N) :- flag(gensym(Prefix),N),!.
oldvalue(_Prefix,0).
set_flag(Name,X) :-
	nonvar(Name),
	retract(flag(Name,Val)),!,
	asserta(flag(Name,X)).
set_flag(Name,X) :-
	nonvar(Name),
	asserta(flag(Name,X)).

:- dynamic flag/2.
flag(foo,foo) :- fail.

:- mode string_concatenate(i,i,o).
string_concatenate(X,Y,XY) :-
   name(X,Xs),name(Y,Ys),stat_append(Xs,Ys,XYs),name(XY,XYs).

:- initialization(stat_menu).
