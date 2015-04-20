:- module(benchmark,_).

/* --------------------------------------------- */
/* (C) COPYRIGHT MICHAEL LEUSCHEL 1995,1996,1997 */
/* --------------------------------------------- */

/* TO GET THE LATEST VERSION OF THE ECCE SYSTEM GO TO:
	http://www.cs.kuleuven.ac.be/~michael */
/* BUG REPORTS, QUESTIONS AND COMMENTS CAN BE SENT TO:
	michael@cs.kuleuven.ac.be */

:- set_prolog_flag(multi_arity_warnings,off).
:- set_prolog_flag(discontiguous_warnings,off).
:- set_prolog_flag(single_var_warnings,off).

:- use_module(library(dec10_io)).
:- use_module(library(prolog_sys)).
:- use_module(library(lists)).

:- use_module(bimtools).
:- use_module(parametric_files).
:- use_module(main_functions).
:- use_module(code_generator).


:- use_module(dynpreds).

/* file: benchmark.pro */

:- include( multi_meta ).

expand_path('$BENCH_DIR/',Dir) :-
	ecce_benchmark_directory(Dir).
expand_path('$ECCE_SPEC_DIR/',SDir) :-
	ecce_benchmark_directory(Dir),
	string_concatenate(Dir,'ecce_out/',SDir).


:- dynamic write_benchmarker_files/1.
write_benchmarker_files(yes).

write_bench :- retract(write_benchmarker_files(_)),fail.
write_bench :- assert(write_benchmarker_files(yes)).

:- dynamic last_bench_path/1.
set_last_bench_path(_New) :-
	retract(last_bench_path(_X)),fail.
set_last_bench_path(New) :-
	assert(last_bench_path(New)).

execute_benchmark(Val) :-
	seen,told,
	(exec_mode(interactive) 
	->(print('Original Program Files will be read from BENCH_DIR:'),nl,
	   expand_path('$BENCH_DIR/',BenchOrigPath),print(BenchOrigPath),nl,
	   print('Specialised Files will be written to ECCE_SPEC_DIR:'),nl,
	   expand_path('$ECCE_SPEC_DIR/',BenchSpecPath),print(BenchSpecPath),nl,
	   nl,
	   print('Benchmark File must contain (in that order):'),nl,
	   print('  orig_prog/1, pd_query/1, run_time_queries/1, run_time_nr/1'),
	   nl,
	   print('  + possibly: test_queries/1, negation_used/1, built_ins_used/1'),nl,
	   print('Path to Benchmark File (BENCH_DIR):'),nl,
	   expand_path('$BENCH_DIR/',BenchPath),
	   print(BenchPath),nl,
	   print('Benchmark File =>'),read(BMFile))
	; BMFile = Val),
	execute_benchmark(BMFile,_).

:- dynamic bs_trans_time/2.

reset_bs_trans_time :-
	retract(bs_trans_time(_X,_Y)),fail.
reset_bs_trans_time.

execute_benchmark_set(Val) :-
	seen,told,
	reset_bs_trans_time,
	(exec_mode(interactive) 
	->(print('Original Program Files will be read from ECCE_ORIG_DIR:'),nl,
	   expand_path('$BENCH_DIR/',BenchOrigPath),print(BenchOrigPath),nl,
	   print('Specialised Files will be written to ECCE_SPEC_DIR:'),nl,
	   expand_path('$ECCE_SPEC_DIR/',BenchSpecPath),print(BenchSpecPath),nl,
	   nl,
	   print('Benchmark Set File must contain:'),nl,
	   print('  benchmark_set/1'),nl,
	   print('Path to Benchmark Set File (BENCH_DIR):'),
	   expand_path('$BENCH_DIR/',BenchPath),print(BenchPath),nl,
	   print('Benchmark Set File (b for benchmarkset.bs, s for benchmarkset_slicing.bs) =>'),
	   read(ReadBMSFile))
	; ReadBMSFile = Val),
	((ReadBMSFile = b)
	 -> (BMSFile = 'benchmarkset.bs')
 	 ;  ((ReadBMSFile = s)
 	    -> (BMSFile = 'benchmarkset_slicing.bs')
 	    ;  (BMSFile = ReadBMSFile)
        	)
	),

	string_concatenate(BenchPath,BMSFile,FullBMSFile),
	see(FullBMSFile),
	(read(benchmark_set(Files)) -> true ; (Files = [])),
	seen,
	generate_csh_command_files(FullBMSFile,Files),
	member(BMFile,Files),
	execute_benchmark(BMFile,TotalTransTime),
	assertz(bs_trans_time(BMFile,TotalTransTime)),
	fail.
execute_benchmark_set(_Val) :-
	nl,print('Transformation Time Summary:'),nl,
	bs_trans_time(File,Time),
	print(File), print('  -->  '), print(Time),nl,
	fail.
execute_benchmark_set(_Val) :-
	nl,print('System parameters: '),print_parameters,nl.


/* name of the log file used during benchmarking of the original
 or specialised programs */
get_bor_log_file_name('bench.time.bor.log').
%get_bor_log_file_name(LogFile) :-
%	expand_path('$BENCH_DIR/',BenchPath),
%        string_concatenate(BenchPath,'bench.time.bor.log',LogFile).
get_bsp_log_file_name('bench.time.bsp.log').
%get_bsp_log_file_name(LogFile) :-
%	expand_path('$BENCH_DIR/',BenchPath),
%        string_concatenate(BenchPath,'bench.time.bsp.log',LogFile).

execute_benchmark(BMFile,TotalTransTime) :-
	expand_path('$BENCH_DIR/',BenchPath),
	string_concatenate(BenchPath,BMFile,FullBMFile),
	see(FullBMFile),
	read(orig_prog(OrigProgFile)),print('.'),
	read(pd_query(PDQuery)),print('.'),
	read(run_time_queries(RunQueries)),print('.'),
	read(run_time_nr(RunNr)),print('.'),
	(read(test_queries(TestQueries)) -> true ; (TestQueries = [])),!,
	print('.'),nl,
	((read(negation_used(NegUsed)),
	  read(built_ins_used(BIUsed)),
	  read(description(Description)))
	 -> (print('Description of benchmark:'),nl,
	     print('Negation:'), print(NegUsed),
	     print(' Built-ins:'), print(BIUsed),nl,
	     print(Description),nl
	    )
	 ;  (print('No Description available'),nl)
	),

	seen,

	expand_path('$BENCH_DIR/',EcceOrigPath),
	clear_database,
	string_concatenate(EcceOrigPath,OrigProgFile,FullOrigProgFile),
	see(FullOrigProgFile),
	read_database,
	seen,
	
	expand_path('$ECCE_SPEC_DIR/',EcceSpecPath),
	string_concatenate(EcceSpecPath,BMFile,FullEcceBMFile),
	string_concatenate(FullEcceBMFile,'.pro',SpecFile),
	set_output_to_file(SpecFile),!,
	garbage_collect, /* perform garbage collection */
	time(pe(PDQuery),TotalTransTime),
	print('Total Transformation Time: '),
	print(TotalTransTime),print(' ms'),nl,
	(write_benchmarker_files(yes)
	 -> (string_concatenate(FullBMFile,'.bsp',BenchSpecFile),
	     get_bsp_log_file_name(BSPLogFile),
	     print('Writing Benchmarker for Specialised Program to: '),
	     print(''''),print(BenchSpecFile),print(''''),nl,
	     tell(BenchSpecFile),
	     (generate_slice_instead_of_spec_prog(no)
	      -> (  filter_queries(TestQueries,TestFQ),
	            print_test(TestFQ,1),
	            print_bench_header(BSPLogFile),
	            filter_queries(RunQueries,RunFQ),
	            print_bench(RunFQ,RunNr)
	         )
	      ;  (  print_test(TestQueries,1),
	            print_bench_header(BSPLogFile),
	            print_bench(RunQueries,RunNr)
	         )
	     ),
	     print(':'), print('- ensure_loaded('),
	     print(''''),print(SpecFile),print(''''),
	     print(').'),nl,
	     print_bench_footer(BMFile,BSPLogFile),
	     told,

	     string_concatenate(FullBMFile,'.bor',BenchOrigFile),
	     get_bor_log_file_name(BORLogFile),
	     print('Writing Benchmarker for Original Program to: '),
	     print(''''),print(BenchOrigFile),print(''''),nl,
	     tell(BenchOrigFile),
	     print_test(TestQueries,1),
	     print_bench_header(BORLogFile),
	     print_bench(RunQueries,RunNr),
	     print(':'), print('- ensure_loaded('),print(''''),
	     print(FullOrigProgFile),print(''''),
	     print(').'),nl,
	     print_bench_footer(BMFile,BORLogFile),
	     told

	     )
	 ; true
	).
execute_benchmark(File,_TotalTransTime) :- seen,told,
	print('Error in Benchmark file:'),print(File),nl.



pe_twice(Q) :-
	/* set_perform_post_msv_analysis(yes), */
	copy(Q,Q2),
	pe(Q),
	clear_database,
	copy_specialised_program_to_input,
	/* set_perform_post_msv_analysis(no), */
	pe(Q2).

filter_queries([],[]).
filter_queries([Goal|T],[FCalls|FT]) :-
	pp_mnf(filter_top_level_call(Goal,FCalls)),
	filter_queries(T,FT).

print_bench([],_Nr).
print_bench([Query|T],Nr) :-
	append(Query,[fail],BenchQuery),
	print_clause_with_nl_nr(Nr,bm,BenchQuery),
	print_bench(T,Nr).

print_test([],_Nr).
print_test([Query|T],Nr) :- 
	pp_mnf(append(Query,[print(Query),nl],TestQuery)),
	print_clause_with_nl(test(Nr),TestQuery),
	Nr1 is Nr + 1,
	print_test(T,Nr1).


print_clause_with_nl_nr(0,_Head,_Body).
print_clause_with_nl_nr(Nr,Head,Body) :-
	Nr>0,
	print_clause_with_nl(Head,Body),
	Nr1 is Nr - 1,
	print_clause_with_nl_nr(Nr1,Head,Body).

print_bench_header(LogFile) :-
	print('bench :'), print('- time_log(bm100,'''),
	print(LogFile),print(''').'),nl,
	print_clause_with_nl_nr(10,bm100,[bm10]),
	print_clause_with_nl(bm100,[]),
	print_clause_with_nl_nr(10,bm10,[bm]).

print_bench_footer(BMFile,LogFile) :-
     print(':'), print('- ensure_loaded(time).'),nl,
     print('main(_) :'), print('- test(X),fail.'),nl,
     print('main(_) :'), print('- add_new_time_log_entry('''),
    print(BMFile),print(''','''),
	print(LogFile),print('''), '),
	print(' bench, bench, bench, halt.'),nl,
    print(':'), print('- main([]).'),nl.


generate_csh_command_files(FullBMSFile,Files) :-
	print('generating csh command files'),nl,
	string_concatenate(FullBMSFile,'.bor.csh',BORfile),
	get_bor_log_file_name(BORLogFile),
	print('-> '),print(BORfile),nl,
	tell(BORfile),
	print('echo Benchmarking original programs'),nl,
	print('echo Resetting Logfile: '),print(BORLogFile),nl,
	print('rm '),print(BORLogFile),nl,
	generate_csh_commands(Files,'.bor'),
	print('echo Finished benchmarking original programs'),nl,
	print('cat '),print(BORLogFile),nl,
	print('echo Examine Logfile for results: '),print(BORLogFile),nl,
	told,
	string_concatenate(FullBMSFile,'.bsp.csh',BSPfile),
	get_bsp_log_file_name(BSPLogFile),
	print('-> '),print(BSPfile),nl,
	tell(BSPfile),
	print('echo Benchmarking specialised programs'),nl,
	print('echo Resetting Logfile: '),print(BSPLogFile),nl,
	print('rm '),print(BSPLogFile),nl,
	generate_csh_commands(Files,'.bsp'),nl,
	print('echo Finished benchmarking specialised programs'),nl,
	print('cat '),print(BSPLogFile),nl,
	print('echo Examine Logfile for results: '),print(BSPLogFile),nl,
	told.


generate_csh_commands([],_).
generate_csh_commands([File|T],Ext) :-
	print('sicstus -l '),
 	%print('ciao '),
 	%print('swipl -c '),
	print(File),print(Ext),nl,
	generate_csh_commands(T,Ext).
