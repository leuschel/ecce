/* file: front_end.pro */

% :- set_prolog_flag(multi_arity_warnings,off).
% :- set_prolog_flag(discontiguous_warnings,off).
% :- set_prolog_flag(single_var_warnings,off).

:- use_module(code_generator,
	[
	    copy_specialised_program_to_input/0,
	    print_clause_with_nl/2,
	    print_specialised_program/0,
	    tcltk_print_sliced_line_numbers/1
	]).
:- use_module(code_generator_isabelle,
	[
	    print_specialised_program_isa/0
	]).
:- use_module(msv_analysis,
	[
	    run_msv_anlysis/0,
	    msv_change/0
	]).
:- use_module(benchmark,
	[
	    execute_benchmark/1,
	    execute_benchmark_set/1
	]).
:- use_module(static_dynamic_functors,
	[
	    toggle_treatment_of_open_predicates/0
	]).
:- use_module(global_tree,
	[
	    print_gt_nodes/0
	]).
:- use_module(ic_gen,
	[
	    gen_ic_code/0
	]).
:- use_module(raf_analysis,
	[
	    perform_raf_analysis/1,
	    perform_raf_analysis/0,
	    perform_andprint_far_analysis/0
	]).
	
%:- use_module(global_tree,
%     [   gt_generate_dot_file/1 ]).
  
:- use_module(dot_generator,
     [   unfold_generate_dot_file/1 ]).
     

/* --------- */
/* FRONT-END */
/* --------- */

:- data last_pd_query/1.
last_pd_query(true).
:- data last_read_file/1.
last_read_file('../append-test').

main(Inputs) :-
	    initialise_parameters,
        (Inputs=[] -> set_exec_mode(interactive),ecce_interactive
         ; set_exec_mode(compiled), ecce_compiled(Inputs)
        ).

ecce :- main([]).

ecce_interactive :-
	perform_self_check,
        please(tw,off),
	init_bd_findall,
	seen,
	see(user),
	told,
	tell(user),
	print('-------------------'),nl,
	print('Welcome to ECCE'),print_ecce_version,nl,
	print('-------------------'),nl,
	print_ecce_release,nl,
	print('Implemented by Michael Leuschel'),nl,
	print('Based on work by Michael Leuschel, Bern Martens, Jesper Jorgensen,'),nl,
	print('Danny De Schreye, Robert Glueck, Morten Heine Sorensen, Andre de Waal,'),nl,
	print('and Mauricio Varea.'),nl,
	print('(C) 1995-2015'),nl,nl,
	print('Interactive Mode'),nl,
	print('type h or ? for help'),nl,
	print('type o to turn off run-time type checking (10x speedup)'),nl,
	front_end([]),
	seen,
	told.

ecce_compiled(Inputs) :- 
    debug_print(ecce_compiled(Inputs)), debug_nl,
        please(tw,off),
	init_bd_findall,
	seen,
	see(user),
	told,
	tell(user),
        process_command_line_options(Inputs),
        seen,
        told.

process_command_line_options(Inputs) :-  set_user_expert(yes), /* set expert mode */
   %print('CLI INPUTS: '), print(Inputs),nl,
   get_options(Inputs,RecognisedOptions,RemOptions),
   %print('Recognised: '), print(RecognisedOptions),nl,
   %print('Remaining: '), print(RemOptions),nl,
   (member(verbose,RecognisedOptions)
     -> set_verbose_printing(on) ; set_verbose_printing(off)),
   verbose_println(process_command_line_options(RecognisedOptions,RemOptions)),
   (member(debug_mode,RecognisedOptions)
     -> (set_debug_printing(on), print(process_command_line_options(RecognisedOptions,RemOptions)),nl) ; true),
   (member(std_config(OptName,ConfigOptNr),RecognisedOptions)
      -> (verbose_print('% Setting online control configuration: '), verbose_print(OptName),verbose_nl,
          set_standard_config(ConfigOptNr)) ; true),
   (member(pp_config(PPOptName,PPConfigOptNr),RecognisedOptions)
      -> (verbose_print('% Setting post-processing configuration: '), verbose_print(PPOptName),verbose_nl,
          set_standard_config(PPConfigOptNr)) ; true),
   set_options(RecognisedOptions),
   (member(perform_self_check,RecognisedOptions) -> perform_self_check ; true),
   (member(help,RecognisedOptions) -> print_help_info ; true),
   (member(profiling,RecognisedOptions) -> profiling_on ; true),
   
   
   (RemOptions = [FileName|Rest] -> (clear_database,
               verbose_println(reading(FileName)),
               catch(front_end_read_in_file(FileName),Exc,
                        (format(user_error,"*** Error reading file '~w'.~n",[FileName]),
                         print_exception(Exc),
                          halt(1)))
             ) 
          ;  ((member(help,RecognisedOptions);member(interactive,RecognisedOptions))
               -> true ; (print('*** Please provide Filename to specialise !'),nl,fail))),
   (Rest=[] -> true ; (print('*** Unkown command line options: '), print(Rest),nl)),
   
   (member(output(File),RecognisedOptions) -> set_output_to_file(File) ; true),
   
   /* The ACTIONS: */
   (member(perform_msv_analysis,RecognisedOptions)
     -> (verbose_println('% Running MSV'),run_msv_anlysis, print_specialised_msv_program_to_file) ; true),
   (member(perform_far_analysis,RecognisedOptions) -> (perform_andprint_far_analysis) ; true),
   (member(perform_raf_analysis(RAFGoal),RecognisedOptions)
     -> (convert_cli_into_atom(RAFGoal,RAFAtom),
         perform_raf_analysis(RAFAtom),
         print_specialised_program_to_file('RAF Analysis Result')) ; true),
   (member(pe(PEGoal),RecognisedOptions)
      -> (convert_cli_into_atom(PEGoal,PEAtom),
          verbose_print('% Partially Evaluating: '), verbose_print(PEAtom),verbose_nl,
          pe_for_front_end(PEAtom,RecognisedOptions)
          ) ; true),
   (member(slice(SliceGoal),RecognisedOptions)
      -> (convert_cli_into_atom(SliceGoal,SliceAtom),
          verbose_print('% Slicing for: '), verbose_print(SliceAtom),verbose_nl,
          set_perform_slice(yes),
          pe_for_front_end(SliceAtom,RecognisedOptions),
          (member(output_slice(OSFile),RecognisedOptions)
            -> tcltk_print_sliced_line_numbers(OSFile)
            ;  true
           ))
      ; true),
   !,
   (error_in_pe_goal_encountered
     -> nl,print('### Undefined call in partial evaluation goal !'),nl
    ; true),
   (member(interactive,RecognisedOptions) -> 
     set_exec_mode(interactive),front_end([]) ; true),
   (member(profiling,RecognisedOptions) -> print_profiling_info ; true).
process_command_line_options(_) :-
   print_help_info.
 
 
print_exception(error(syntax_error(StartLine,EndLine,Err,Str),_Pred)) :- !,
  format(user_error,"*** Syntax Error, Lines ~w-~w.~n",[StartLine,EndLine]),
  write(user_error,'*** '), write(user_error,Err),nl(user_error),
  write(user_error,'*** '), write(user_error,Str),nl(user_error).
print_exception(E) :- write(user_error,'*** '),
  write(user_error,E),nl(user_error).
  

set_options(RecognisedOptions) :-
   member(set_option(Call),RecognisedOptions),
   (call(Call)-> true ; (print('### Error in setting FLAG'),nl,
                         print('### '),print(set_option_failed(Call)),nl,nl)),
   fail.
set_options(_).

pe_for_front_end(PEAtom,RecognisedOptions) :-
	statistics(runtime,[Global1,_]),
    pe_for_front_end2(PEAtom,RecognisedOptions), % having problems in Ciao with calling time/1 here
	statistics(runtime,[Global2,_TimeSinceLastStat]),
	Time is Global2 - Global1,
    print('% Total time for specialisation: '),print(Time),print(' ms'),nl.
pe_for_front_end2(PEAtom,RecognisedOptions) :-
          pe_without_pp(PEAtom,RealPEGoal),
          gen_dot_file(RecognisedOptions),
          pe_post_process(RealPEGoal).
gen_dot_file(RecognisedOptions) :-
  (member(dot_output(DotFile),RecognisedOptions)
      -> (verbose_print('% Generating Dot Output: '), verbose_print(DotFile),verbose_nl,
          unfold_generate_dot_file(DotFile)) ; true).
          

profiling_on :- set_prolog_flag(profiling,on), print('% PROFILING ON'),nl.
print_profiling_info :-
 on_exception(error(existence_error(_,_),_),print_profile, print('CAN ONLY BE USED WHEN RUNNING ECCE FROM SOURCE')),nl.
 
print_help_info :-
   print('ECCE: The online partial evaluator for pure Prolog'),nl,
   print('       (c) Michael Leuschel 1995-2005'),nl,
   print('USAGE: '),nl,
   print('ecce [OPTIONS] FILE'),nl,
   print(' OPTIONS:'),nl,
   print('   -pe "GOAL"    partially evaluate FILE for GOAL'),nl,
   print('   -slice "GOAL" slice FILE for GOAL'),nl,
   print('   -msv          run MSV Analysis on FILE'),nl,
   print('   -raf GOAL     run RAF Argument Filtering on FILE for GOAL'),nl,  
   print('   -far          run FAR Argument Filtering on FILE'),nl,  
   print('   -o FILE       write specialised program to FILE'),nl,
   print('   -os FILE      write slicing information to FILE'),nl,
   print('   -dot FILE     write specialization graph to dot FILE (before post-processing)'),nl,
   print('   -v            verbose mode'),nl, 
   print('   -t            perform self-test'),nl, 
   print('   -d            print debugging information'),nl, 
   print('   -i            stay in interactive mode after performing OPTIONS'),nl,
   print('   -config OPT   change default control settings, OPT=classic,fast,'),nl,
   print('                 mixtus,minimal,classic-fast,term,deforest,karpmiller,finkel,termdet'),nl,
   print('   -pp OPT       change default postprocessor settings, OPT=off,max'),nl,
   print('   -FLAG yes/no  change individual settings, FLAG=abstract_parent,pp_unf,pp_dce,pp_reduce,pp_raf,pp_far,pp_msv'),nl.

get_options([],Rec,Rem) :- !,Rec=[],Rem=[].
get_options(Inputs,RecognisedOptions,RemOptions) :-
   (recognise_option(Inputs,Flag,RemInputs)
     -> RecognisedOptions = [Flag|RecO2], RemO2 = RemOptions
     ;  Inputs = [H|RemInputs], RemOptions = [H|RemO2], RecO2 = RecognisedOptions
   ),
   get_options(RemInputs,RecO2,RemO2).

recognise_option(Inputs,Flag,RemInputs) :-
   recognised_option(Heads,Flag),
   append(Heads,RemInputs,Inputs).
   
recognised_option(['-t'],perform_self_check).
recognised_option(['-i'],interactive).
recognised_option(['-d'],debug_mode).
recognised_option(['-v'],verbose).
recognised_option(['-s',Sel],selectionrule(S)).
recognised_option(['-msv'],perform_msv_analysis).
recognised_option(['-far'],perform_far_analysis).
recognised_option(['-raf', Goal],perform_raf_analysis(Goal)).
recognised_option(['-pe',Goal],pe(Goal)).
recognised_option(['-slice',Goal],slice(Goal)).
recognised_option(['-o',File],output(File)).
recognised_option(['-os',File],output_slice(File)).
recognised_option(['-dot',File],dot_output(File)).
recognised_option(['-config',Option],std_config(Option,OptNumber)) :- translate_std_config(Option,OptNumber).
recognised_option(['-pp',Option],pp_config(Option,OptNumber)) :- translate_postprocessing(Option,OptNumber).
recognised_option(['-abstract_parent',Option],set_option(set_perform_ppa(Option))).
recognised_option(['-pp_unf',Option],set_option(set_perform_determinate_post_unfolding(Option))).
recognised_option(['-pp_dce',Option],set_option(set_perform_dce(Option))).
recognised_option(['-pp_reduce',Option],set_option(set_reduce_polyvariance(Option))).
recognised_option(['-pp_raf',Option],set_option(set_perform_raf(Option))).
recognised_option(['-pp_far',Option],set_option(set_perform_far(Option))).
recognised_option(['-pp_msv',Option],set_option(set_perform_post_msv_analysis(Option))).
recognised_option(['--help'],help).
recognised_option(['-help'],help).
recognised_option(['-h'],help).
recognised_option(['-profile'],profiling).


translate_std_config('classic',98).
translate_std_config('classic-fast',102).
translate_std_config('fast',100).
translate_std_config('minimal',112).
translate_std_config('mixtus',120).
translate_std_config('deforest',103).
translate_std_config('term',term). /* best std. PD + termination preserving unfolding rule + no rrc */
translate_std_config('termdet',termd). 

translate_std_config('karpmiller',121).
translate_std_config('finkel',122).


translate_postprocessing('off',110).
translate_postprocessing('max',111).

  
% add a dot at the end; in case user forgets
add_dot([],".").
add_dot(".",".") :- !.
add_dot([A|T],[A|R]) :- add_dot(T,R).
  
safe_front_end_read_in_file(Filename) :-
        catch(front_end_read_in_file(FileName),Exc,
             (format(user_error,"*** Error reading file '~w'.~n",[FileName]),
                         print_exception(Exc))).
                          
front_end_read_in_file(Filename) :-
   read_in_file(Filename),
   retract_fact(last_read_file(_)),
   assertz_fact(last_read_file(Filename)).
   
   
front_end([]) :- !,
	prompt(_OldPrompt,''),
	print('=> '),
	ecce_get(AsciiChar),
 	(action(AsciiChar)
	-> true
	;  format('Unknown command ~w, type h or ? for help',[AsciiChar]),nl,
	   front_end([])
	).
front_end([A|P]) :-
        char_code(A,Action),debug_print('::'),debug_print(action(Action)),debug_nl,
 	(action(Action,P)
	-> true
	;  format('Unknown command ~w, try "ecce h" or "ecce ?" for help',[Action]),nl
	).


list_database :-
	claus(Nr,Head,Body),
	print(Nr),print(': '),
	print_clause_with_nl(Head,Body),fail.
list_database :-
	next_free_mode_nr(MNr),
	(MNr > 1
	-> MT is MNr - 1,
	   print('number of mode declarations: '),print(MT),nl
	; true
	).

:- set_prolog_flag(multi_arity_warnings,off).

action(X):- action(X,[]).

action(10,P) :- % newline 
   front_end(P).
action(13,P) :- front_end(P).
action(98,P):- 
	(exec_mode(compiled) -> P=[RR|PP] ; P=PP),
	execute_benchmark(RR),  /* b for Benchmark */
	front_end(PP).
action(99,P):- /* c for clear clause database */
	clear_database,
	set_make_iff_definitions_read_in(no),
	front_end(P).
action(100,P):- /* d for determinate post-unfolding */
	perform_determinate_post_unfolding, front_end(P).
action(101,P):- /* e for dEbugging */
	(exec_mode(compiled) -> (P=[F|RR],RR=[S|PP]) ; P=PP),
	set_debug_printing_value(F), set_trace_printing_value(S), front_end(PP).
action(102,P) :- /* f */
	(exec_mode(compiled) -> P=[RR|PP] ; P=PP),
	set_output_to_file_int(RR),front_end(PP).
action(103,P):- /* g for print global tree */
	print('Global Tree:'),nl,print_gt_nodes, front_end(P).
action(105,P):- /* i for insert specialised program into clause db */
	copy_specialised_program_to_input,
	print_claus_database_status,
	front_end(P).
action(106,P):- /* j create ic-checking clauses */
	gen_ic_code,
	front_end(P).
action(107,P) :- /* k for benchmarkset */
	(exec_mode(compiled) -> P=[RR|PP] ; P=PP),
	execute_benchmark_set(RR), front_end(PP).
action(108,P) :- /* l for listing */
	print(' Clauses in database:'),nl,
	list_database,
	front_end(P).
action(109,P) :- /* m for msv analysis */
	run_msv_anlysis,
	print('More Specific Version of Program:'),nl,
	print_specialised_program,
	(msv_change
		-> print('New information was derived.')
		;  print('No new information !')
	),nl,
	front_end(P).
action(110,P) :- /* n: enable abstract partial deduction */
	set_standard_config(97),
	front_end(P).
action(97,P) :- /* a: toggle treatment of open predicates */
	toggle_treatment_of_open_predicates,
	front_end(P).
action(112,P):- /* p: partially evaluate an atom */
	last_pd_query(Last),
	(exec_mode(interactive) 
	-> (beginner_print('Use list notation for goals: [G1,G2].'),beginner_nl,
	    beginner_print('Type a dot (.) and hit return at the end.'),beginner_nl,
	    print('atom or goal (l for '),print(Last),print(') =>'))
	; true ),
	(P=[] -> read(R),PP=[] ; P=[RR|PP],read_atom(RR,R)),
	(R=l -> Atom = Last
	    ;   Atom = R,retract_fact(last_pd_query(_)),
	        assertz_fact(last_pd_query(Atom))
	),
	time(main_functions:pe(Atom),T),nl,
	print('Full transformation time (with code generation): '), print(T),
	print(' ms'),nl,
	front_end(PP).
action(111,P) :-  /* o: NOT WORKING YET! needs the implementation of a dispatcher for 
	             bimtools:prepost (also allocating each part of bimtools into 
                     separate modules would be a good idea!)*/
	ecce_reconsult('bimtools/prepost.nocheck.pl'),
	display( P ) , nl ,nl,
	front_end(P).
action(113,_). /* q for quit */
action(114,P):- /* r for read in file into clause database */
	last_read_file(Last),
	(exec_mode(interactive) 
	->(beginner_print('Type a dot (.) and hit return at the end.'),beginner_nl,
	   print('filename (l for '),print(Last),print(') =>'))
	; true ),
	(P=[]-> read(R),PP=[] ; P=[R|PP]),
	(R=l -> Filename = Last ;  Filename = R),
	safe_front_end_read_in_file(Filename),
	front_end(PP).
action(115,P):- /* s for set parameters */
	set_parameters, front_end(P).
action(116,P):- /* t  */
	set_make_iff_when_reading_clauses, front_end(P).
action(117,P):- /* u  */
	manual_unfold, front_end(P).
action(118,P):- /* v  */
	toggle_user_expert, front_end(P).
action(119,P) :- /* w for Write to file */
	print_specialised_program_to_file,
	front_end(P).
action(87,P) :- /* eld: W for Write to Isabelle file */
    print_specialised_program_isa,
	front_end(P).
action(121,P):- /* y  */
	perform_raf_analysis, front_end(P).
action(122,P):- /* z  */
	perform_andprint_far_analysis, front_end(P).

action(120,_) :- stop. /* x for exit */

action(63,_P) :- action(104). /* ? for help  */
action(104,P) :- /* h for help */
	print(' ECCE 2.0'),nl,
	print(' The Partial Evaluator based on Characteristic Atoms and Global Trees'),nl,nl, print('  '),
	print_claus_database_status,nl,
	print(' Start system with "ecce --help" to get help on command-line non-interactive use.'),nl,
	print(' Command Summary for interactive mode:'),nl,
	expert_print('  b: execute Benchmark (from special file)'),expert_nl,
	print('  c: Clear clause database'),nl,
	print('  e: set dEbugging on/off'),nl,
	print('  f: choose File for output'),nl,
	print('  h: Help (also ?)'),nl,
	print('  i: Insert specialised program into clause database'),nl,
	expert_print('  k: execute benchmarK Set(from special file)'),expert_nl,
	print('  l: List clause database'),nl,
	print('  o: turn type checking Off'),nl,
	print('  p: Partially evaluate an atom or goal'),nl,
	print('  r: Read clauses into database'),nl,
	print('  s: Set Parameters'),nl,
	print('  v: set user expert leVel on/off'),nl,
	print('  w: Write specialised program to file'),nl,
    print('  W: Write specialised program as Isabelle theory file'),nl, /* eld */
	print('  x: eXit (also a and q)'),nl,
	print('  ----------------------------------- '),nl,
	expert_print('  a: toggle treatment of open predicates'),expert_nl,
	print('  d: Determinate (post-)unfolding'),nl,
	expert_print('  g: print Global tree'),expert_nl,
	expert_print('  j: generate ic-checking code'),expert_nl,
	print('  m: Msv analysis'),nl,
	print('  n: eNable abstract partial deduction'),nl,
	expert_print('  t: iff clause Transformation'),expert_nl,
	expert_print('  u: manual Unfold'),expert_nl,
	expert_print('  y: redundant argument filtering (RAF) analYsis'),expert_nl,
	expert_print('  z: FAR (reversed RAF) analysis'),expert_nl,
	(exec_mode(interactive)->front_end(P);true).

:- set_prolog_flag(multi_arity_warnings,on).


