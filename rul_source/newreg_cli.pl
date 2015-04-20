/************************************************************************/
/* Copyright (C)1994-99 University of Bristol                           */
/*                                                                      */
/*                                                                      */
/*  Copyright Notice:                                                   */
/*  -----------------                                                   */
/*                                                                      */
/*  This software and its documentation is subjected to copyright.      */
/*  It is available free of charge provided a request is made to        */
/*  the implementors (for example by sending a message to               */
/*  john@compsci.bristol.ac.uk).                                        */
/*  Permission is granted to make (but not distribute, in order to      */
/*  keep a coherent thread of development) copies of this software as   */
/*  received or modified, in any medium, provided that the copyright    */
/*  notice and permission notice are preserved.                         */
/*                                                                      */
/************************************************************************/
/*******************************/
/*  Copyright (C)1994-99       */
/*  JPG/University of Bristol  */
/*                             */
/*                             */
/* Regular Approximation Tool  */
/*                             */
/* John Gallagher              */
/* Dept. of Computer Science   */
/* University of Bristol       */
/* Bristol, UK                 */
/*                             */
/* john@compsci.bristol.ac.uk  */
/*                             */
/* version 6/7/99             */
/*                             */
/*******************************/

/* CLI options added by Michael Leuschel */


runtime_entry(start) :- go.

save :- save_program('rul.sav').

:- use_module(library(charsio),[read_from_chars/2]).
:- use_module(library(system),[system/1]).
:- use_module(library(lists)).
:- use_module(library(timeout)).


:- dynamic options/1.

option(X) :- options(List), member(X,List).

go :-
    prolog_flag(argv,ArgV),
    get_options(ArgV,Opts,AX),
    assert(options(Opts)),
    
    (AX = [F,Query|_RestArgv] 
     -> (name(Query,AsciiL),add_dot(AsciiL,AL2),read_from_chars(AL2,Q),
         timeout_call(td(F,Q),Opts))
     ;  (AX=[F]
          -> (member(bup,Opts)
               -> timeout_call(bu(F),Opts)
               ;  (print('Query to specialise =>'), read(Query),
                   timeout_call(td(F,Query),Opts))
              )
          ;  fail
        )
    ),
    halt.
go :- print('A tool for the analysis of logic programs using Regular Unary Logic Programs'),nl,
      print('Developed by John Gallagher'),nl,
      print('Usage: rul [Switches] File.pl ["Atom."]'),nl,
      print('Switches: '),nl,
      print('          -b:  do bottom-up goal independent analysis' ),nl,      
      print('          -u:  find useless clauses' ),nl,           
      print('          -o FILE: specify output file' ),nl,    
      print('          -t value: timeout after value ms' ),nl,           
      print('Note:     the . after Atom are optional'),nl.
    



timeout_call(Call,Opts) :-
 (get_timeout(Opts,TOut)
  -> (time_out(Call,TOut,Result),
      (Result=success -> true
         ; (print('*** TIMEOUT !!!'),nl,
            print('*** Analysis took more than '),print(TOut),print(' ms'),nl,
            print('***'),nl )
     ))
  ;  Call).

get_timeout(Opts,TOut) :-
   member(timeout(OF),Opts),
   name(OF,AsciiL),add_dot(AsciiL,AL2),read_from_chars(AL2,TOut),
   TOut>0, TOut=<2147483646.
 
% add a dot at the end; in case user forgets
add_dot([],".").
add_dot(".",".") :- !.
add_dot([A|T],[A|R]) :- add_dot(T,R).

get_options([],[],[]).
get_options([X|T],Options,Args) :-
   (recognised_option(X,Opt,Values) ->       
       ( append(Values, Rest, T),
	 RT = Rest,  
	 Options = [Opt|OT], Args = AT
       )   
   ;
       (
	 Options = OT,     Args = [X|AT],
	 RT = T
       )
   ),
   get_options(RT,OT,AT).

recognised_option('-b',bup,[]).
recognised_option('-u',find_useless,[]).
recognised_option('-o',output_file(OF),[OF]).
recognised_option('-t',timeout(OF),[OF]).


:- dynamic options/1.

/*******************************/

:- dynamic program_preds/1.
:- dynamic program_pred/2.
:- dynamic ub_clause/2.
:- dynamic directive/1.
:- op(900,fy,not).
:- dynamic trace_on/0.
:- dynamic atom_trace/0.
:- dynamic neg/1.
:- dynamic evaluable/1.
:- dynamic traceon/0.
:- dynamic numexpr/1.
:- dynamic numeric/1.

bu :- 
      new_line_1,
      write('From File: '),
      read(X1),
      new_line_1,
      bu(X1).
      
bu(File) :- 
      write('% Doing Bottom-Up Goal-Independent RUL Analysis'),
      sp_ttynl_1,
      sp_consult_1(File),
      program_preds(X2),
      tp_fix_1(X2).
td :- 
      new_line_1,
      write('From File: '),
      read(X1),
      new_line_1,
      write('Query: '),
      read(X2),
      td(X1,X2).
td(X1,X2) :- 
      write('% Doing Goal-Dependent RUL Analysis using Magic Set Transformation'),
      sp_ttynl_1,
      magic_1(X1,X2),
      sp_consult_1('/tmp/query_answer_temp_file'),
      program_preds(X3),
      tp_fix_1(X3).
      
new_line_1 :- 
      nl,
      tab(6).
sp_consult_1(X1) :- 
      sp_ttynl_1,
      nofileerrors,
      see(X1),
      fileerrors,
      abolish_all_existing_1,
      read(X2),
      read_clauses_1(X2,X3,1),
      seen,
      sp_ttynl_1,
      write(user,'% Finished reading '),
      write(user,X1),
      sp_ttynl_1,
      assert(program_preds(X3)),
      strong_connected_components_1(X3,X4),
      put_sccs_1(X4),
      !.
sp_consult_1(X1) :- 
      name(X1,X2),
      name('.pl',X3),
      file_suffix_1(X2,X3,X4),
      name(X5,X4),
      nofileerrors,
      see(X5),
      fileerrors,
      abolish_all_existing_1,
      read(X6),
      read_clauses_1(X6,X7,1),
      seen,
      sp_ttynl_1,
      write(user,'% Finished reading '),
      write(user,X5),
      sp_ttynl_1,
      assert(program_preds(X7)),
      strong_connected_components_1(X7,X8),
      put_sccs_1(X8),
      !.
sp_consult_1(X1) :- 
      seen,
      assert(program_preds([])),
      write(user,'Cannot find file '),
      write(user,X1),
      write(user,', or problems while reading.'),
      sp_ttynl_1,
      sp_ttynl_1.
tp_fix_1(X1) :- 
      time(tp_fix1_1(X1,X2)),
      display_fix_1(X2).
magic_1(X1,X2) :- 
      tell('/tmp/query_answer_temp_file'),
      query_answer_clauses_1(X1,X2),
      told.
sp_ttynl_1 :- 
      ttynl.
abolish_all_existing_1 :- 
      !,
      write(user,'% Clearing workspace....'),
      abolish(ub_clause/2),
      abolish(directive/1),
      assert(directive(dummy)),
      abolish(program_preds/1),
      abolish(program_pred/2).
abolish_all_existing_1 :- 
      true.
read_clauses_1(end_of_file,X1,X2) :- 
      !,
      write(user,'% Number of clauses: '),
      write(user,X2),
      sp_ttynl_1,
      close_list_1(X1).
read_clauses_1((:-entry(X1,X2)),X3,X4) :- 
      !,
      assert(directive(entry(X1))),
      read(X5),
      read_clauses_1(X5,X3,X4).
read_clauses_1((:-dynamic X1/X2),X3,X4) :- 
      !,
      assert(directive((dynamic X1/X2))),
      functor(X5,X1,X2),
      assert(ub_clause(imported(X5),true)),
      read(X6),
      read_clauses_1(X6,X3,X4).
read_clauses_1((:-X1),X2,X3) :- 
      !,
      call(X1),
      assert(directive(X1)),
      read(X4),
      !,
      read_clauses_1(X4,X2,X3).
read_clauses_1(X1,X2,X3) :- 
      get_pred_name_1(X1,X4,X5),
      each_memb1_1([X4|X5],X2),
      store_clause_1(X1),
      read(X6),
      !,
      X7 is X3+1,
      read_clauses_1(X6,X2,X7).
strong_connected_components_1([],[]) :- 
      !.
strong_connected_components_1(X1,X2) :- 
      step1_1(X1,X3),
      step2_1(X3,X2),
      recursive_classify_1(X2).
put_sccs_1([]) :- 
      true.
put_sccs_1([(X1,X2)|X3]) :- 
      put_scc_1(X1,X2),
      put_sccs_1(X3).
file_suffix_1([],X1,X1) :- 
      true.
file_suffix_1([X1|X2],X3,[X1|X4]) :- 
      file_suffix_1(X2,X3,X4).
tp_fix1_1(X1,X2) :- 
      strong_connected_components_1(X1,X3),
      nl,
      tp_fix_groups_1(X3,[],X2,10,X4),
      show_useless_clauses_1(X1,X2,X4,X5).
display_fix_1(X1) :- 
      nl,
      %write('Output file: '),
      %read(X2),
      (option(output_file(File)) -> X2 = File ; X2 = tty),
      reg_filewrite_1(X2,X1).
query_answer_clauses_1(X1,X2) :- 
      see(X1),
      read(X3),
      left_ans_clauses_1(X3,X4),
      seen,
      see(X1),
      read(X5),
      right_ans_clauses_1(X5),
      seen,
      see(X1),
      read(X6),
      left_query_clauses_1(X6),
      seen,
      see(X1),
      read(X7),
      right_query_clauses_1(X7),
      seen,
      write_ans_clauses_1(X4),
      canonical_1(X2),
      flat_name_1(left_query(X2),X8),
      flat_name_1(right_query(X2),X9),
      displayqRClauses_1([(X8:-true)]),
      nl,
      displayqRClauses_1([(X9:-true)]),
      nl.
close_list_1([]) :- 
      !.
close_list_1([X1|X2]) :- 
      close_list_1(X2).
get_pred_name_1((X1:-X2),X3/X4,X5) :- 
      !,
      functor(X1,X3,X4),
      body_preds_1(X2,X5).
get_pred_name_1(X1,X2/X3,[]) :- 
      functor(X1,X2,X3).
each_memb1_1([],X1) :- 
      true.
each_memb1_1([X1|X2],X3) :- 
      memb1_1(X1,X3),
      each_memb1_1(X2,X3).
store_clause_1((X1:-X2)) :- 
      !,
      assert(ub_clause(X1,X2)).
store_clause_1(X1) :- 
      assert(ub_clause(X1,true)).
step1_1(X1,state([X2|X3],X2,[],0)) :- 
      init_depends_1(X1,[X2|X3]).
step2_1(X1,X2) :- 
      next_vertex_1(X1,X3),
      step3_1(X3,X2).
recursive_classify_1([]) :- 
      true.
recursive_classify_1([(recursive,[X1,X2|X3])|X4]) :- 
      !,
      recursive_classify_1(X4).
recursive_classify_1([(recursive,[X1])|X2]) :- 
      direct_recursive_1(X1),
      !,
      recursive_classify_1(X2).
recursive_classify_1([(non_recursive,X1)|X2]) :- 
      recursive_classify_1(X2).
put_scc_1(X1,[]) :- 
      true.
put_scc_1(X1,[X2|X3]) :- 
      assert(program_pred(X2,X1)),
      put_scc_1(X1,X3).
tp_fix_groups_1([],X1,X1,X2,X2) :- 
      !.
tp_fix_groups_1([(X1,X2)|X3],X4,X5,X6,X7) :- 
      nonrec_1(X1),
      write(X2),
      nl,
      !,
      init_1(X2,X8),
      tp_alpha_1(X2,[[]],X4,X8,X9,X10,X6,X11),
      app_1(X4,X9,X12),
      tp_fix_groups_1(X3,X12,X5,X11,X7).
tp_fix_groups_1([(X1,X2)|X3],X4,X5,X6,X7) :- 
      write(X2),
      nl,
      init_1(X2,X8),
      tp_iterate_1([[]],X2,X4,X8,X9,X6,X10),
      app_1(X4,X9,X11),
      tp_fix_groups_1(X3,X11,X5,X10,X7).
show_useless_clauses_1(X1,X2,X3,X4) :- 
      new_line_1,
      new_line_1,
      %write('Find useless clauses (y/n)? '),
      %getchar_1(X5),
      (option(find_useless) -> X5 = y ; X5 = n),
      show_useless_clauses_2(X5,X1,X2,X3,X4).
reg_filewrite_1(tty,X1) :- 
      !,
      nl,
      write('% Regular Approximation Program:'),
      nl,
      nl,
      displayqTypes_1(X1),
      nl,
      nl.
reg_filewrite_1(X1,X2) :- 
      tell(X1),
      displayqTypes_1(X2),
      told.
left_ans_clauses_1(X1,X2) :- 
      end_of_file_1(X1),
      !.
left_ans_clauses_1((:-X1),X2) :- 
      !,
      read(X3),
      !,
      left_ans_clauses_1(X3,X2).
left_ans_clauses_1((X1:-X2),X3) :- 
      !,
      left_answerlits_1(X2,X4),
      flat_name_1('_left_ans'(X1),X5),
      flat_name_1(left_query(X1),X6),
      flat_names_1(X4,X7),
      canonical_1((X5:-X6,X7)),
      displayqRClauses_1([(X5:-X6,X7)]),
      functor(X1,X8,X9),
      memb1_1(X8/X9,X3),
      read(X10),
      !,
      left_ans_clauses_1(X10,X3).
left_ans_clauses_1(X1,X2) :- 
      flat_name_1('_left_ans'(X1),X3),
      flat_name_1(left_query(X1),X4),
      canonical_1((X3:-X4)),
      displayqRClauses_1([(X3:-X4)]),
      functor(X1,X5,X6),
      memb1_1(X5/X6,X2),
      read(X7),
      !,
      left_ans_clauses_1(X7,X2).
right_ans_clauses_1(X1) :- 
      end_of_file_1(X1),
      !.
right_ans_clauses_1((:-X1)) :- 
      !,
      read(X2),
      !,
      right_ans_clauses_1(X2).
right_ans_clauses_1((X1:-X2)) :- 
      !,
      right_answerlits_1(X2,X3),
      flat_name_1('_right_ans'(X1),X4),
      flat_name_1(right_query(X1),X5),
      flat_names_1(X3,X6),
      canonical_1((X4:-X5,X6)),
      displayqRClauses_1([(X4:-X5,X6)]),
      read(X7),
      !,
      right_ans_clauses_1(X7).
right_ans_clauses_1(X1) :- 
      flat_name_1('_right_ans'(X1),X2),
      flat_name_1(right_query(X1),X3),
      canonical_1((X2:-X3)),
      displayqRClauses_1([(X2:-X3)]),
      read(X4),
      !,
      right_ans_clauses_1(X4).
left_query_clauses_1(X1) :- 
      end_of_file_1(X1),
      !.
left_query_clauses_1((X1:-X2)) :- 
      !,
      bodylit_left_queries_1(X1,X2),
      read(X3),
      !,
      left_query_clauses_1(X3).
left_query_clauses_1(X1) :- 
      read(X2),
      !,
      left_query_clauses_1(X2).
right_query_clauses_1(X1) :- 
      end_of_file_1(X1),
      !.
right_query_clauses_1((X1:-X2)) :- 
      !,
      bodylit_right_queries_1(X1,X2),
      read(X3),
      !,
      right_query_clauses_1(X3).
right_query_clauses_1(X1) :- 
      read(X2),
      !,
      right_query_clauses_1(X2).
write_ans_clauses_1([]) :- 
      !.
write_ans_clauses_1([X1/X2|X3]) :- 
      functor(X4,X1,X2),
      canonical_1(X4),
      flat_name_1(ans(X4),X5),
      flat_name_1('_left_ans'(X4),X6),
      flat_name_1('_right_ans'(X4),X7),
      displayqRClauses_1([(X5:-X6,X7)]),
      !,
      write_ans_clauses_1(X3).
canonical_1(X1) :- 
      canonical_2(X1).
flat_name_1(X1,X1) :- 
      sp_builtin_1(X1),
      !.
flat_name_1(X1,X2) :- 
      X1=..[X3,X4],
      X4=..[X5|X6],
      name(X3,X7),
      name(X5,X8),
      concat_1(X8,X7,X9),
      name(X10,X9),
      X2=..[X10|X6].
displayqRClauses_1([]) :- 
      true.
displayqRClauses_1([X1|X2]) :- 
      melt_1(X1,X3),
      sp_writeRclause_1(X3),
      displayqRClauses_1(X2).
body_preds_1(true,[]) :- 
      !.
body_preds_1((\+X1,X2),X3) :- 
      !,
      body_preds_1((X1,X2),X3).
body_preds_1((X1,X2),X3) :- 
      sp_builtin_1(X1),
      !,
      body_preds_1(X2,X3).
body_preds_1((X1,X2),[X3/X4|X5]) :- 
      !,
      functor(X1,X3,X4),
      body_preds_1(X2,X5).
body_preds_1(\+X1,X2) :- 
      !,
      body_preds_1(X1,X2).
body_preds_1(X1,[]) :- 
      sp_builtin_1(X1),
      !.
body_preds_1(X1,[X2/X3]) :- 
      functor(X1,X2,X3).
memb1_1(X1,[X1|X2]) :- 
      !.
memb1_1(X1,[X2|X3]) :- 
      memb1_1(X1,X3).
init_depends_1([X1|X2],[vertex(X1,X3,0,0,undef)|X4]) :- 
      immed_depends_1(1,X1,[],X3),
      init_depends_1(X2,X4).
init_depends_1([],[]) :- 
      true.
next_vertex_1(state(X1,vertex(X2,X3,X4,X5,X6),X7,X8),state(X9,vertex(X2,X3,X10,X10,X6),[X2|X7],X10)) :- 
      X10 is X8+1,
      update_vertex_1(X2,X3,X10,X10,X6,X1,X9).
step3_1(X1,X2) :- 
      no_unused_edges_1(X1),
      !,
      step7_1(X1,X2).
step3_1(X1,X2) :- 
      next_edge_1(X1,X3,X4),
      step4_1(X4,X3,X2).
direct_recursive_1(X1/X2) :- 
      functor(X3,X1,X2),
      ub_clause(X3,X4),
      rec_body_1(X1,X2,X4).
nonrec_1(non_recursive) :- 
      true.
init_1([X1|X2],[[]|X3]) :- 
      init_1(X2,X3).
init_1([],[]) :- 
      true.
tp_alpha_1([],X1,X2,[],[],[],X3,X3) :- 
      true.
tp_alpha_1([X1|X2],X3,X4,[X5|X6],[X7|X8],[X9|X10],X11,X12) :- 
      user_clauses_1(X1,X13),
      infer_proc_1(X13,X3,X4,X14,X11,X15),
      ub_proc_1(X14,X5,X5,X7,X9,X15,X16),
      tp_alpha_1(X2,X3,X4,X6,X8,X10,X16,X12).
app_1([],X1,X1) :- 
      true.
app_1([X1|X2],X3,[X1|X4]) :- 
      app_1(X2,X3,X4).
tp_iterate_1([],X1,X2,X3,X3,X4,X4) :- 
      !.
tp_iterate_1(X1,X2,X3,X4,X5,X6,X7) :- 
      writestate_1(X4),
      app_1(X4,X3,X8),
      tp_alpha_1(X2,X1,X8,X4,X9,X10,X6,X11),
      fold_1(X9,X12,X10,X11,X13),
      countdefs_1(X12),
      check_fix_1(X2,X10,X14),
      !,
      tp_iterate_1(X14,X2,X3,X12,X5,X13,X7).
getchar_1(X1) :- 
      ttyget(X2),
      name(X1,[X2]).
show_useless_clauses_2(y,X1,X2,X3,X4) :- 
      new_line_1,
      write('% The following clauses are useless:'),
      new_line_1,
      new_line_1,
      !,
      useless_clause_check_1(X1,X2,X3,X4,X5),
      new_line_1,
      write('% The following predicates are useless:'),
      new_line_1,
      write(X5).
show_useless_clauses_2(X1,X2,X3,X4,X4) :- 
      true.
displayqTypes_1([[]|X1]) :- 
      displayqTypes_1(X1).
displayqTypes_1([[X1|X2]|X3]) :- 
      displayqPreddef_1(X1),
      displayqTypes_1(X2),
      displayqTypes_1(X3).
displayqTypes_1([proc(X1,X2)|X3]) :- 
      displayqRClauses_1(X2),
      displayqTypes_1(X3).
displayqTypes_1([]) :- 
      true.
end_of_file_1(end_of_file) :- 
      true.
left_answerlits_1((X1,X2),(X1,X3)) :- 
      sp_builtin_1(X1),
      !,
      left_answerlits_1(X2,X3).
left_answerlits_1((X1,X2),('_left_ans'(X1),X3)) :- 
      !,
      left_answerlits_1(X2,X3).
left_answerlits_1(X1,X1) :- 
      sp_builtin_1(X1).
left_answerlits_1(X1,'_left_ans'(X1)) :- 
      true.
flat_names_1((X1,X2),(X3,X4)) :- 
      !,
      flat_name_1(X1,X3),
      flat_names_1(X2,X4).
flat_names_1(X1,X2) :- 
      flat_name_1(X1,X2).
right_answerlits_1((X1,X2),(X1,X3)) :- 
      sp_builtin_1(X1),
      !,
      right_answerlits_1(X2,X3).
right_answerlits_1((X1,X2),('_right_ans'(X1),X3)) :- 
      !,
      right_answerlits_1(X2,X3).
right_answerlits_1(X1,X1) :- 
      sp_builtin_1(X1).
right_answerlits_1(X1,'_right_ans'(X1)) :- 
      true.
bodylit_left_queries_1(X1,true) :- 
      !.
bodylit_left_queries_1(X1,X2) :- 
      removelast_1(X2,X3,X4),
      sp_builtin_1(X3),
      !,
      bodylit_left_queries_1(X1,X4).
bodylit_left_queries_1(X1,X2) :- 
      removelast_1(X2,X3,X4),
      !,
      left_answerlits_1(X4,X5),
      flat_name_1(left_query(X3),X6),
      flat_names_1((left_query(X1),X5),X7),
      canonical_1((X6:-X7)),
      displayqRClauses_1([(X6:-X7)]),
      bodylit_left_queries_1(X1,X4).
bodylit_right_queries_1(X1,true) :- 
      !.
bodylit_right_queries_1(X1,X2) :- 
      removefirst_1(X2,X3,X4),
      sp_builtin_1(X3),
      !,
      bodylit_right_queries_1(X1,X4).
bodylit_right_queries_1(X1,X2) :- 
      removefirst_1(X2,X3,X4),
      !,
      right_answerlits_1(X4,X5),
      flat_name_1(right_query(X3),X6),
      flat_names_1((right_query(X1),X5),X7),
      canonical_1((X6:-X7)),
      displayqRClauses_1([(X6:-X7)]),
      bodylit_right_queries_1(X1,X4).
canonical_2(X1) :- 
      name('X',[X2]),
      mynumbervars_1(X1,X2,1,X3).
sp_builtin_1(X1) :- 
      predicate_property(X1,built_in).
concat_1([],X1,[95|X1]) :- 
      true.
concat_1([X1|X2],X3,[X1|X4]) :- 
      concat_1(X2,X3,X4).
melt_1(X1,X2) :- 
      melt1_1(X1,X2,X3),
      !.
sp_writeRclause_1((X1:-X2)) :- 
      canonical_1((X1:-X2)),
      write(X1),
      write(' :- '),
      writeqRBody_1(X2).
immed_depends_1(0,X1,X2,X2) :- 
      true.
immed_depends_1(X1,X2/X3,X4,X5) :- 
      X1>0,
      user_clauses_1(X2/X3,X6),
      body_preds_2(X1,X6,X4,X5).
update_vertex_1(X1,X2,X3,X4,X5,[vertex(X1,X6,X7,X8,X9)|X10],[vertex(X1,X2,X3,X4,X5)|X10]) :- 
      !.
update_vertex_1(X1,X2,X3,X4,X5,[X6|X7],[X6|X8]) :- 
      update_vertex_1(X1,X2,X3,X4,X5,X7,X8).
no_unused_edges_1(state(X1,vertex(X2,[],X3,X4,X5),X6,X7)) :- 
      true.
step7_1(X1,[(X2,X3)|X4]) :- 
      lowpoint_1(X1,X5),
      !,
      component_1(X1,X5,X6,X3),
      step8_1(X6,X4).
step7_1(X1,X2) :- 
      step8_1(X1,X2).
next_edge_1(state(X1,vertex(X2,[X3|X4],X5,X6,X7),X8,X9),X10,state(X11,vertex(X2,X4,X5,X6,X7),X8,X9)) :- 
      get_vertex_1(X3,X1,X10),
      update_vertex_1(X2,X4,X5,X6,X7,X1,X11).
step4_1(X1,X2,X3) :- 
      new_vertex_1(X2,X1,X4),
      !,
      step2_1(X4,X3).
step4_1(X1,X2,X3) :- 
      step5_1(X1,X2,X3).
rec_body_1(X1,X2,(X3,X4)) :- 
      functor(X3,X1,X2),
      !.
rec_body_1(X1,X2,(X3,X4)) :- 
      !,
      rec_body_1(X1,X2,X4).
rec_body_1(X1,X2,X3) :- 
      functor(X3,X1,X2).
user_clauses_1(X1/X2,X3) :- 
      functor(X4,X1,X2),
      findall((X4:-X5),ub_clause(X4,X5),X3),
      !.
user_clauses_1(X1,[]) :- 
      true.
infer_proc_1([(X1:-X2)|X3],X4,X5,X6,X7,X8) :- 
      nochange_1(X2,X4),
      !,
      infer_proc_1(X3,X4,X5,X6,X7,X8).
infer_proc_1([(X1:-X2)|X3],X4,X5,[X6|X7],X8,X9) :- 
      tsolve_1(X2,X5,X10,X11),
      unfold_body_1(X10,X11,X12),
      intersect_body_1(X12,X11,X13,X14,X8,X15),
      !,
      headtype_1(X1,X13,X16,X17,X15,X18),
      app_1(X14,X11,X19),
      app_1(X17,X19,X6),
      infer_proc_1(X3,X4,X5,X7,X18,X9).
infer_proc_1([X1|X2],X3,X4,X5,X6,X7) :- 
      infer_proc_1(X2,X3,X4,X5,X6,X7).
infer_proc_1([],X1,X2,[],X3,X3) :- 
      true.
ub_proc_1([],X1,X2,X2,nochange,X3,X3) :- 
      !.
ub_proc_1([X1],X2,[proc(X3/1,X4)|X5],X6,nochange,X7,X7) :- 
      toptype_1(X1,X8),
      app_1(X1,X2,X9),
      subtype_1(X8,X3,X9),
      !,
      rdefs_1(X3,X6,X9).
ub_proc_1([X1],X2,[proc(X3/1,X4)|X5],X6,change,X7,X8) :- 
      !,
      toptype_1(X1,X9),
      app_1(X1,X2,X10),
      ub1_1(X3,X9,X11,X12,X10,X7,X8),
      app_1(X12,X10,X13),
      rdefs_1(X11,X6,X13).
ub_proc_1([X1],X2,X3,X4,change,X5,X5) :- 
      !,
      toptype_1(X1,X6),
      app_1(X1,X2,X7),
      rdefs_1(X6,X4,X7).
ub_proc_1([X1,X2|X3],X4,X5,X6,X7,X8,X9) :- 
      toptype_1(X1,X10),
      toptype_1(X2,X11),
      app_1(X2,X4,X12),
      app_1(X1,X12,X13),
      subtype_1(X10,X11,X13),
      !,
      app_1(X2,X4,X14),
      ub_proc_1([X2|X3],X14,X5,X6,X7,X8,X9).
ub_proc_1([X1,X2|X3],X4,X5,X6,X7,X8,X9) :- 
      toptype_1(X1,X10),
      toptype_1(X2,X11),
      app_1(X2,X4,X12),
      app_1(X1,X12,X13),
      subtype_1(X11,X10,X13),
      !,
      app_1(X1,X4,X14),
      ub_proc_1([X1|X3],X14,X5,X6,X7,X8,X9).
ub_proc_1([X1,X2|X3],X4,X5,X6,X7,X8,X9) :- 
      toptype_1(X1,X10),
      toptype_1(X2,X11),
      app_1(X2,X4,X12),
      app_1(X1,X12,X13),
      ub1_1(X10,X11,X14,X15,X13,X8,X16),
      app_1(X15,X13,X17),
      ub_proc_1([X15|X3],X17,X5,X6,X7,X16,X9).
writestate_1(X1) :- 
      traceon,
      write(X1),
      read(X2),
      !,
      nl.
writestate_1(X1) :- 
      write('% *'),
      sp_ttyflush_1.
fold_1([],[],[],X1,X1) :- 
      true.
fold_1([X1|X2],[X1|X3],[nochange|X4],X5,X6) :- 
      !,
      fold_1(X2,X3,X4,X5,X6).
fold_1([X1|X2],[X3|X4],[change|X5],X6,X7) :- 
      toptype_1(X1,X8),
      rdefs_1(X8,X9,X1),
      foldp_1(X9,X3,X6,X10),
      fold_1(X2,X4,X5,X10,X7).
countdefs_1([X1|X2]) :- 
      sp_length_1(X1,X3),
      write(X3),
      write(' '),
      !,
      countdefs_1(X2).
countdefs_1([]) :- 
      nl.
check_fix_1([],[],[]) :- 
      true.
check_fix_1([X1|X2],[change|X3],[X1|X4]) :- 
      check_fix_1(X2,X3,X4).
check_fix_1([X1|X2],[nochange|X3],X4) :- 
      check_fix_1(X2,X3,X4).
useless_clause_check_1([],X1,X2,X2,[]) :- 
      true.
useless_clause_check_1([X1|X2],X3,X4,X5,X6) :- 
      user_clauses_1(X1,X7),
      check_bodies_1(X1,1,X7,X3,X4,X8,X9),
      useless_pred_1(X1,X9,X6,X10),
      useless_clause_check_1(X2,X3,X8,X5,X10).
displayqPreddef_1(proc(X1,[(X2:-X3)])) :- 
      X2=..[X4,X5],
      displayqRClauses_1([(X5:-X3)]).
removelast_1((X1,X2,X3),X4,(X1,X5)) :- 
      !,
      removelast_1((X2,X3),X4,X5).
removelast_1((X1,X2),X2,X1) :- 
      !.
removelast_1(X1,X1,true) :- 
      true.
removefirst_1((X1,X2),X1,X2) :- 
      !.
removefirst_1(X1,X1,true) :- 
      true.
mynumbervars_1(X1,X2,X3,X4) :- 
      var(X1),
      !,
      X4 is X3+1,
      name(X3,X5),
      name(X1,[X2|X5]).
mynumbervars_1(X1,X2,X3,X3) :- 
      atomic(X1),
      !.
mynumbervars_1(X1,X2,X3,X4) :- 
      X1=..[X5|X6],
      mynumbervarlist_1(X6,X2,X3,X4).
melt1_1(X1,X2,X3) :- 
      variable_1(X1),
      !,
      assoc_1(X1,X2,X3).
melt1_1(X1,X1,X2) :- 
      atomic(X1),
      !.
melt1_1(X1,X2,X3) :- 
      functor(X1,X4,X5),
      functor(X2,X4,X5),
      meltargs_1(1,X5,X1,X2,X3).
writeqRBody_1((X1,X2)) :- 
      !,
      write(X1),
      write(','),
      writeqRBody_1(X2).
writeqRBody_1(X1) :- 
      write(X1),
      write('.'),
      nl.
body_preds_2(X1,[(X2:-X3)|X4],X5,X6) :- 
      bodylits_1(X1,X3,X5,X7),
      body_preds_2(X1,X4,X7,X6).
body_preds_2(X1,[],X2,X2) :- 
      true.
lowpoint_1(state(X1,vertex(X2,X3,X4,X4,X5),X6,X7),X2) :- 
      true.
component_1(state(X1,X2,X3,X4),X5,state(X1,X2,X6,X4),X7) :- 
      pop_1(X3,X5,X6,X7).
step8_1(X1,X2) :- 
      definedFV_1(X1),
      !,
      newLV1_1(X1,X3),
      step3_1(X3,X2).
step8_1(X1,X2) :- 
      step9_1(X1,X2).
get_vertex_1(X1,[vertex(X1,X2,X3,X4,X5)|X6],vertex(X1,X2,X3,X4,X5)) :- 
      !.
get_vertex_1(X1,[X2|X3],X4) :- 
      get_vertex_1(X1,X3,X4).
new_vertex_1(vertex(X1,X2,0,X3,X4),state(X5,vertex(X6,X7,X8,X9,X10),X11,X12),state(X13,vertex(X1,X2,0,X3,X6),X11,X12)) :- 
      update_vertex_1(X1,X2,0,X3,X6,X5,X13).
step5_1(X1,X2,X3) :- 
      forward_edge_1(X2,X1),
      !,
      step3_1(X1,X3).
step5_1(X1,X2,X3) :- 
      different_component_1(X1,X2),
      !,
      step3_1(X1,X3).
step5_1(X1,X2,X3) :- 
      step6_1(X1,X2,X3).
nochange_1(X1,[[]]) :- 
      !,
      fail.
nochange_1(true,X1) :- 
      !.
nochange_1(X1,X2) :- 
      preds_1(X1,X3),
      \+commonpred_1(X3,X2).
tsolve_1(true,X1,true,[]) :- 
      !.
tsolve_1((X1,X2),X3,X4,X5) :- 
      !,
      tsolve_1(X1,X3,X6,X7),
      tsolve_1(X2,X3,X8,X9),
      app_1(X7,X9,X5),
      joingoals_1(X6,X8,X4).
tsolve_1(X1=X2,X3,true,[]) :- 
      !,
      X1=X2.
tsolve_1(X1,X2,X3,[]) :- 
      sp_builtin_1(X1),
      !,
      builtin_type_1(X1,X3).
tsolve_1(X1,X2,X3,X4) :- 
      functor(X1,X5,X6),
      headclause_1(X5,X6,X2,X7,X8,X4),
      melt_1((X7:-X8),(X9:-X3)),
      X9=..[X10,X1].
unfold_body_1(true,X1,true) :- 
      !.
unfold_body_1((X1,X2),X3,X4) :- 
      !,
      unfold_body_1(X1,X3,X5),
      unfold_body_1(X2,X3,X6),
      joingoals_1(X5,X6,X4).
unfold_body_1(X1,X2,X1) :- 
      X1=..[X3,X4],
      var(X4),
      !.
unfold_body_1(X1,X2,X3) :- 
      functor(X1,any,1),
      !,
      any_term_1(X1,X3).
unfold_body_1(X1,X2,X3) :- 
      X1=..[X4,X5],
      typeclause_1(X4,X2,X6,X7),
      matches_1(X6,X7,X1,X8),
      unfold_body_1(X8,X2,X3).
intersect_body_1(true,X1,true,[],X2,X2) :- 
      !.
intersect_body_1((X1,X2),X3,X4,X5,X6,X7) :- 
      reoccurs_1(X1,X2,X8,X9),
      !,
      X1=..[X10,X11],
      X8=..[X12,X11],
      intersect_1(X10,X12,X13,X14,X3,X6,X15),
      X16=..[X13,X11],
      app_1(X14,X3,X17),
      intersect_body_1((X16,X9),X17,X4,X18,X15,X7),
      app_1(X14,X18,X5).
intersect_body_1((X1,X2),X3,(X1,X4),X5,X6,X7) :- 
      !,
      intersect_body_1(X2,X3,X4,X5,X6,X7).
intersect_body_1(X1,X2,X1,[],X3,X3) :- 
      true.
headtype_1(X1,X2,X3,[proc(X3/1,[(X4:-X5)])|X6],X7,X8) :- 
      functor(X1,X9,X10),
      functor(X11,X9,X10),
      newname_1(X7,X3),
      X4=..[X3,X11],
      canonical_1(X4),
      X12 is X7+1,
      argtypes_1(X1,X2,X11,X5,1,X10,X12,X8,X6).
toptype_1([proc(X1/1,X2)|X3],X1) :- 
      true.
subtype_1(X1,X2,X3) :- 
      subtype_2(X1,X2,[],X4,X3).
rdefs_1(X1,X2,X3) :- 
      iterate_defs_1([X1],X1,X4,X2,X3).
ub1_1(X1,X2,X3,[proc(X3/1,X4)|X5],X6,X7,X8) :- 
      def_1(X1,X9,X6),
      def_1(X2,X10,X6),
      newname_1(X7,X3),
      X11 is X7+1,
      !,
      upper_type_1(X9,X10,X4,X3,X1,X2,X3,X12,X13,X11,X14,X6),
      iterate_ub_1(X13,X1,X2,X3,X12,X5,X14,X8,X6).
sp_ttyflush_1 :- 
      ttyflush.
foldp_1(X1,X2,X3,X4) :- 
      head_functors_1(X1,X5),
      X5=[X6,X7|X8],
      same_head_functors_1(X9,X10,X5),
      depends_1(X9,X10,X11,X1),
      subtype_1(X10,X9,X1),
      !,
      recursive_1(X9,X10,X11,X1,X12,X3,X13),
      toptype_1(X12,X14),
      rdefs_1(X14,X15,X12),
      foldp_1(X15,X2,X13,X4).
foldp_1(X1,X1,X2,X2) :- 
      true.
sp_length_1(X1,X2) :- 
      length(X1,X2).
check_bodies_1(X1,X2,[(X3:-true)|X4],X5,X6,X7,[X2|X8]) :- 
      !,
      X9 is X2+1,
      check_bodies_1(X1,X9,X4,X5,X6,X7,X8).
check_bodies_1(X1,X2,[(X3:-X4)|X5],X6,X7,X8,[X2|X9]) :- 
      tsolve_1(X4,X6,X10,X11),
      unfold_body_1(X10,X11,X12),
      intersect_body_1(X12,X11,X13,X14,X7,X15),
      !,
      X16 is X2+1,
      check_bodies_1(X1,X16,X5,X6,X15,X8,X9).
check_bodies_1(X1,X2,[(X3:-X4)|X5],X6,X7,X8,X9) :- 
      canonical_1((X3:-X4)),
      write(X1),
      write(': Clause '),
      write(X2),
      new_line_1,
      X10 is X2+1,
      check_bodies_1(X1,X10,X5,X6,X7,X8,X9).
check_bodies_1(X1,X2,[],X3,X4,X4,[]) :- 
      true.
useless_pred_1(X1,[],[X1|X2],X2) :- 
      !.
useless_pred_1(X1,X2,X3,X3) :- 
      true.
mynumbervarlist_1([],X1,X2,X2) :- 
      true.
mynumbervarlist_1([X1|X2],X3,X4,X5) :- 
      mynumbervars_1(X1,X3,X4,X6),
      mynumbervarlist_1(X2,X3,X6,X5).
variable_1(X1) :- 
      variable_2(X1,X2).
assoc_1(X1,X2,[assoc(X1,X2)|X3]) :- 
      !.
assoc_1(X1,X2,[X3|X4]) :- 
      assoc_1(X1,X2,X4).
meltargs_1(X1,X2,X3,X4,X5) :- 
      X1>X2,
      !.
meltargs_1(X1,X2,X3,X4,X5) :- 
      arg(X1,X3,X6),
      melt1_1(X6,X7,X5),
      arg(X1,X4,X7),
      X8 is X1+1,
      meltargs_1(X8,X2,X3,X4,X5).
bodylits_1(X1,(X2,X3),X4,X5) :- 
      \+sp_builtin_1(X2),
      !,
      functor(X2,X6,X7),
      insertp_1(X6,X7,X4,X8),
      X9 is X1-1,
      immed_depends_1(X9,X6/X7,X8,X10),
      bodylits_1(X1,X3,X10,X5).
bodylits_1(X1,(X2,X3),X4,X5) :- 
      !,
      bodylits_1(X1,X3,X4,X5).
bodylits_1(X1,X2,X3,X4) :- 
      \+sp_builtin_1(X2),
      !,
      functor(X2,X5,X6),
      insertp_1(X5,X6,X3,X7),
      X8 is X1-1,
      immed_depends_1(X8,X5/X6,X7,X4).
bodylits_1(X1,X2,X3,X3) :- 
      true.
pop_1([X1|X2],X1,X2,[X1]) :- 
      !.
pop_1([X1|X2],X3,X4,[X1|X5]) :- 
      pop_1(X2,X3,X4,X5).
definedFV_1(state(X1,vertex(X2,X3,X4,X5,X6),X7,X8)) :- 
      \+X6=undef.
newLV1_1(state(X1,vertex(X2,X3,X4,X5,X6),X7,X8),state(X9,vertex(X6,X10,X11,X12,X13),X7,X8)) :- 
      get_vertex_1(X6,X1,vertex(X6,X10,X11,X14,X13)),
      minimum_1(X14,X5,X12),
      update_vertex_1(X6,X10,X11,X12,X13,X1,X9).
step9_1(X1,X2) :- 
      unused_vertex_1(X1,X3),
      !,
      step2_1(X3,X2).
step9_1(X1,X2) :- 
      step10_1(X1,X2).
forward_edge_1(vertex(X1,X2,X3,X4,X5),state(X6,vertex(X7,X8,X9,X10,X11),X12,X13)) :- 
      X3>X9.
different_component_1(state(X1,X2,X3,X4),vertex(X5,X6,X7,X8,X9)) :- 
      \+memb1_1(X5,X3).
step6_1(X1,X2,X3) :- 
      newLV_1(X1,X2,X4),
      step3_1(X4,X3).
preds_1(true,[]) :- 
      !.
preds_1((X1,X2),[X3/X4|X5]) :- 
      !,
      functor(X1,X3,X4),
      preds_1(X2,X5).
preds_1(X1,[X2/X3]) :- 
      functor(X1,X2,X3).
commonpred_1(X1,X2) :- 
      memb2_1(X3,X1),
      memb2_1(X3,X2).
joingoals_1(true,X1,X1) :- 
      !.
joingoals_1(X1,true,X1) :- 
      !.
joingoals_1((true,X1),X2,X3) :- 
      !,
      joingoals_1(X1,X2,X3).
joingoals_1((X1,true),X2,X3) :- 
      !,
      joingoals_1(X1,X2,X3).
joingoals_1((X1,X2),X3,(X1,X4)) :- 
      !,
      joingoals_1(X2,X3,X4).
joingoals_1(X1,X2,(X1,X2)) :- 
      X1=..[X3|X4],
      X3\==','.
builtin_type_1(X1 is X2,(numeric(X1),numexpr(X2))) :- 
      !.
builtin_type_1(X1<X2,(numexpr(X1),numexpr(X2))) :- 
      !.
builtin_type_1(X1>X2,(numexpr(X1),numexpr(X2))) :- 
      !.
builtin_type_1(X1=<X2,(numexpr(X1),numexpr(X2))) :- 
      !.
builtin_type_1(X1>=X2,(numexpr(X1),numexpr(X2))) :- 
      !.
builtin_type_1(number(X1),numeric(X1)) :- 
      !.
builtin_type_1(integer(X1),numeric(X1)) :- 
      !.
builtin_type_1(X1,X2) :- 
      any_term_1(X1,X2).
headclause_1(X1,X2,X3,X4,X5,X6) :- 
      functor(X7,X1,X2),
      hclause_1(X7,X3,X4,X5,X6).
any_term_1(X1,X2) :- 
      vars_1(X1,X3),
      any_vars_1(X3,X2).
typeclause_1(X1,X2,X3,X4) :- 
      def_1(X1,X5,X2),
      !,
      memb2_1((X3:-X4),X5).
matches_1(X1,X2,X3,X4) :- 
      melt_1((X1:-X2),(X3:-X4)),
      !.
matches_1(X1,true,X2,true) :- 
      X1=..[X3,X4],
      basic_type_1(X4),
      X2=..[X5,X6],
      atomic(X6),
      type_check_1(X6,X4).
reoccurs_1(X1,(X2,X3),X2,X3) :- 
      X1=..[X4,X5],
      X2=..[X6,X7],
      X5==X7,
      !.
reoccurs_1(X1,(X2,X3),X4,(X2,X5)) :- 
      !,
      reoccurs_1(X1,X3,X4,X5).
reoccurs_1(X1,X2,X2,true) :- 
      X1=..[X3,X4],
      X2=..[X5,X6],
      X4==X6,
      !.
intersect_1(X1,X2,X1,[],X3,X4,X4) :- 
      subtype_1(X1,X2,X3),
      !.
intersect_1(X1,X2,X2,[],X3,X4,X4) :- 
      subtype_1(X2,X1,X3),
      !.
intersect_1(X1,X2,X3,[proc(X3/1,X4)|X5],X6,X7,X8) :- 
      def_1(X1,X9,X6),
      def_1(X2,X10,X6),
      newname_1(X7,X3),
      X11 is X7+1,
      !,
      lower_type_1(X9,X10,X4,X3,X1,X2,X3,X12,X13,X11,X14,X6),
      iterate_intersect_1(X13,X1,X2,X3,X12,X5,X14,X8,X6).
newname_1(X1,X2) :- 
      name(X1,X3),
      name(t,X4),
      app_1(X4,X3,X5),
      name(X2,X5).
argtypes_1(X1,X2,X3,true,X4,0,X5,X5,[]) :- 
      true.
argtypes_1(X1,X2,X3,X4,X5,X5,X6,X7,X8) :- 
      argtype_1(X1,X2,X3,X4,X5,X6,X7,X8).
argtypes_1(X1,X2,X3,(X4,X5),X6,X7,X8,X9,X10) :- 
      X6<X7,
      argtype_1(X1,X2,X3,X4,X6,X8,X11,X12),
      X13 is X6+1,
      argtypes_1(X1,X2,X3,X5,X13,X7,X11,X9,X14),
      app_1(X12,X14,X10).
subtype_2(X1,any,X2,X2,X3) :- 
      !.
subtype_2(X1,X1,X2,X2,X3) :- 
      !.
subtype_2(X1,X2,X3,X3,X4) :- 
      loopsubtype_1(X1,X2,X3),
      !.
subtype_2(X1,X2,X3,X4,X5) :- 
      X1\==any,
      X2\==any,
      def_1(X1,X6,X5),
      def_1(X2,X7,X5),
      !,
      subtype1_1(X6,X7,[(X1,X2)|X3],X4,X5).
iterate_defs_1([],X1,X2,[],X3) :- 
      !.
iterate_defs_1(X1,X2,X3,X4,X5) :- 
      each_def_1(X1,X4,X6,X2,X3,X7,X5),
      iterate_defs_1(X7,X2,X3,X6,X5).
def_1(numexpr,X1,X2) :- 
      !,
      findall((numexpr(X3):-X4),clause(numexpr(X3),X4),X1).
def_1(numeric,X1,X2) :- 
      !,
      findall((numeric(X3):-X4),clause(numeric(X3),X4),X1).
def_1(X1,X2,X3) :- 
      def1_1(X1,X2,X3),
      !.
def_1(X1,X2,X3) :- 
      user_clauses_1(X1/1,X2),
      canonical_1(X2),
      X2\==[],
      !.
def_1(X1,X2,X3) :- 
      write(X1/1),
      write(' not found'),
      nl,
      fail.
upper_type_1([],[],[],X1,X2,X3,X4,X5,X6,X7,X7,X8) :- 
      !.
upper_type_1([],[(X1:-X2)|X3],[(X4:-X2)|X5],X6,X7,X8,X9,X10,X11,X12,X13,X14) :- 
      !,
      X1=..[X15,X16],
      X4=..[X6,X16],
      upper_type_1([],X3,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14).
upper_type_1([(X1:-X2)|X3],[],[(X4:-X2)|X5],X6,X7,X8,X9,X10,X11,X12,X13,X14) :- 
      !,
      X1=..[X15,X16],
      X4=..[X6,X16],
      upper_type_1(X3,[],X5,X6,X7,X8,X9,X10,X11,X12,X13,X14).
upper_type_1([(X1:-true)|X2],X3,[(X4:-true)|X5],X6,X7,X8,X9,X10,X11,X12,X13,X14) :- 
      X1=..[X15,X16],
      basic_type_1(X16),
      !,
      remove_type_instances_1(X16,X3,X17),
      X4=..[X6,X16],
      upper_type_1(X2,X17,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14).
upper_type_1([(X1:-true)|X2],X3,[(X4:-true)|X5],X6,X7,X8,X9,X10,X11,X12,X13,X14) :- 
      X1=..[X15,X16],
      basic_type_instance_1(X16,X3,X17,X18),
      !,
      remove_type_instances_1(X17,X2,X19),
      X4=..[X6,X17],
      upper_type_1(X19,X18,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14).
upper_type_1([(X1:-X2)|X3],X4,[(X5:-X6)|X7],X8,X9,X10,X11,X12,X13,X14,X15,X16) :- 
      removeclause1_1(X1,X17,X4,X18),
      !,
      X1=..[X19,X20],
      X5=..[X8,X20],
      ub_body_1(X2,X17,X6,X9,X10,X11,X12,X13,X14,X21,X16),
      upper_type_1(X3,X18,X7,X8,X9,X10,X11,X12,X13,X21,X15,X16).
upper_type_1([(X1:-X2)|X3],X4,[(X5:-X2)|X6],X7,X8,X9,X10,X11,X12,X13,X14,X15) :- 
      X1=..[X16,X17],
      X5=..[X7,X17],
      upper_type_1(X3,X4,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15).
iterate_ub_1([],X1,X2,X3,X4,[],X5,X5,X6) :- 
      !.
iterate_ub_1(X1,X2,X3,X4,X5,X6,X7,X8,X9) :- 
      each_ub_1(X1,X10,X11,X2,X3,X4,X5,X7,X12,X9),
      iterate_ub_1(X11,X2,X3,X4,X5,X13,X12,X8,X9),
      app_1(X10,X13,X6).
head_functors_1([proc(X1,X2)|X3],X4) :- 
      head_funcs1_1(X3,X4).
same_head_functors_1(X1,X2,X3) :- 
      two_membs_1(X1,X4,X2,X3),
      X1\==X2,
      \+more_funcs_1(X1,X2,X3),
      \+more_funcs_1(X2,X1,X3).
depends_1(X1,X2,X3,[X4|X5]) :- 
      nested_memb_1(X1,X6,X5),
      iterate_defs_1([X1],X1,X7,X3,X5),
      makelist_1(X7),
      memb2_1(X2,X7).
recursive_1(X1,X2,X3,X4,X5,X6,X6) :- 
      normalise_defs_1(X3,X2,X1,X7),
      replace_procs_1(X7,X4,X5).
variable_2(X1,X2) :- 
      atom(X1),
      name('X',[X3]),
      name(X1,[X3|X4]),
      name(X2,X4),
      integer(X2).
insertp_1(X1,X2,X3,X3) :- 
      memb1_1(X1/X2,X3),
      !.
insertp_1(X1,X2,X3,[X1/X2|X3]) :- 
      true.
minimum_1(X1,X2,X1) :- 
      X1=<X2.
minimum_1(X1,X2,X2) :- 
      X1>X2.
unused_vertex_1(state(X1,X2,X3,X4),state(X1,X5,X3,X4)) :- 
      newstart_1(X1,X5).
step10_1(X1,[]) :- 
      true.
newLV_1(state(X1,vertex(X2,X3,X4,X5,X6),X7,X8),vertex(X9,X10,X11,X12,X13),state(X14,vertex(X2,X3,X4,X15,X6),X7,X8)) :- 
      minimum_1(X5,X11,X15),
      update_vertex_1(X2,X3,X4,X15,X6,X1,X14).
memb2_1(X1,[X1|X2]) :- 
      true.
memb2_1(X1,[X2|X3]) :- 
      memb2_1(X1,X3).
vars_1(X1,X2) :- 
      vars_2(X1,[],X2).
any_vars_1([],true) :- 
      !.
any_vars_1([X1],any(X1)) :- 
      !.
any_vars_1([X1|X2],(any(X1),X3)) :- 
      any_vars_1(X2,X3).
basic_type_1(numeric) :- 
      true.
type_check_1(X1,numeric) :- 
      number(X1).
lower_type_1([],X1,[],X2,X3,X4,X5,X6,X7,X8,X8,X9) :- 
      !.
lower_type_1(X1,[],[],X2,X3,X4,X5,X6,X7,X8,X8,X9) :- 
      !.
lower_type_1([(X1:-true)|X2],X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13) :- 
      X1=..[X14,X15],
      basic_type_1(X15),
      !,
      remove_any_type_instance_1(X15,X2,X3,X1,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13).
lower_type_1([(X1:-X2)|X3],X4,[(X5:-X6)|X7],X8,X9,X10,X11,X12,X13,X14,X15,X16) :- 
      removeclause_1(X1,X17,X4,X18),
      !,
      X1=..[X19,X20],
      X5=..[X8,X20],
      intersect_body_2(X2,X17,X6,X9,X10,X11,X12,X13,X14,X21,X16),
      lower_type_1(X3,X18,X7,X8,X9,X10,X11,X12,X13,X21,X15,X16).
lower_type_1([X1|X2],X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13) :- 
      lower_type_1(X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13).
iterate_intersect_1([],X1,X2,X3,X4,[],X5,X5,X6) :- 
      !.
iterate_intersect_1(X1,X2,X3,X4,X5,X6,X7,X8,X9) :- 
      each_intersect_1(X1,X10,X11,X2,X3,X4,X5,X7,X12,X9),
      iterate_intersect_1(X11,X2,X3,X4,X5,X13,X12,X8,X9),
      app_1(X10,X13,X6).
argtype_1(X1,X2,X3,X4,X5,X6,X6,[]) :- 
      arg(X5,X1,X7),
      var(X7),
      !,
      arg(X5,X3,X8),
      giventype_1(X7,X2,X9),
      X4=..[X9,X8].
argtype_1(X1,X2,X3,X4,X5,X6,X7,X8) :- 
      arg(X5,X1,X9),
      arg(X5,X3,X10),
      termtype_1(X9,X2,X8,X6,X7),
      newname_1(X6,X11),
      X4=..[X11,X10].
subtype1_1([],X1,X2,X2,X3) :- 
      true.
subtype1_1([(X1:-X2)|X3],X4,X5,X6,X7) :- 
      removeclause_1(X1,X8,X4,X9),
      subtype_pairs_1(X2,X8,X5,X10,X7),
      subtype1_1(X3,X9,X10,X6,X7).
each_def_1([],X1,X1,X2,X3,X4,X5) :- 
      !.
each_def_1([X1|X2],[proc(X1/1,X3)|X4],X5,X6,X7,X8,X9) :- 
      def_1(X1,X3,X9),
      body_defs_1(X3,X6,X7,X8,X9),
      each_def_1(X2,X4,X5,X6,X7,X8,X9).
def1_1(X1,X2,[proc(X1/1,X2)|X3]) :- 
      !.
def1_1(X1,X2,[proc(X3,X4)|X5]) :- 
      def1_1(X1,X2,X5).
remove_type_instances_1(X1,[],[]) :- 
      true.
remove_type_instances_1(X1,[(X2:-true)|X3],X4) :- 
      X2=..[X5,X6],
      type_check_1(X6,X1),
      !,
      remove_type_instances_1(X1,X3,X4).
remove_type_instances_1(X1,[(X2:-true)|X3],X4) :- 
      X2=..[X5,X1],
      !,
      remove_type_instances_1(X1,X3,X4).
remove_type_instances_1(X1,[X2|X3],[X2|X4]) :- 
      remove_type_instances_1(X1,X3,X4).
basic_type_instance_1(X1,[(X2:-true)|X3],X4,X3) :- 
      X2=..[X5,X4],
      basic_type_1(X4),
      type_check_1(X1,X4),
      !.
basic_type_instance_1(X1,[X2|X3],X4,[X2|X5]) :- 
      basic_type_instance_1(X1,X3,X4,X5).
removeclause1_1(X1,X2,[(X3:-X2)|X4],X4) :- 
      X1=..[X5,X6],
      X3=..[X7,X8],
      functor(X6,X9,X10),
      functor(X8,X9,X10),
      !.
removeclause1_1(X1,X2,[X3|X4],[X3|X5]) :- 
      removeclause1_1(X1,X2,X4,X5).
ub_body_1(true,true,true,X1,X2,X3,X4,X5,X6,X6,X7) :- 
      !.
ub_body_1((X1,X2),(X3,X4),(X5,X6),X7,X8,X9,X10,X11,X12,X13,X14) :- 
      !,
      ub_atom_1(X1,X3,X5,X7,X8,X9,X10,X11,X12,X15,X14),
      ub_body_1(X2,X4,X6,X7,X8,X9,X10,X11,X15,X13,X14).
ub_body_1(X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11) :- 
      ub_atom_1(X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11).
each_ub_1([],[],X1,X2,X3,X4,X5,X6,X6,X7) :- 
      !.
each_ub_1([(X1,X2,X3)|X4],[proc(X3/1,X5)|X6],X7,X8,X9,X10,X11,X12,X13,X14) :- 
      def_1(X1,X15,X14),
      def_1(X2,X16,X14),
      X17 is X12+1,
      !,
      upper_type_1(X15,X16,X5,X3,X8,X9,X10,X11,X7,X17,X18,X14),
      each_ub_1(X4,X6,X7,X8,X9,X10,X11,X18,X13,X14).
head_funcs1_1([],[]) :- 
      true.
head_funcs1_1([proc(X1/1,X2)|X3],X4) :- 
      clause_head_functors_1(X1,X2,X4,X5),
      head_funcs1_1(X3,X5).
two_membs_1(X1,X2,X3,[head_func(X1,X2)|X4]) :- 
      memb2_1(head_func(X3,X2),X4).
two_membs_1(X1,X2,X3,[X4|X5]) :- 
      two_membs_1(X1,X2,X3,X5).
more_funcs_1(X1,X2,X3) :- 
      memb2_1(head_func(X1,X4),X3),
      \+memb2_1(head_func(X2,X4),X3).
nested_memb_1(X1,X2,[proc(X1/1,X2)|X3]) :- 
      true.
nested_memb_1(X1,X2,[X3|X4]) :- 
      nested_memb_1(X1,X2,X3).
nested_memb_1(X1,X2,[X3|X4]) :- 
      nested_memb_1(X1,X2,X4).
makelist_1([]) :- 
      !.
makelist_1([X1|X2]) :- 
      makelist_1(X2).
normalise_defs_1([],X1,X2,[]) :- 
      true.
normalise_defs_1([proc(X1,X2)|X3],X4,X5,[proc(X1,X6)|X7]) :- 
      normalise_1(X2,X4,X5,X6),
      normalise_defs_1(X3,X4,X5,X7).
replace_procs_1([],X1,X1) :- 
      true.
replace_procs_1([X1|X2],X3,X4) :- 
      replace_proc_1(X1,X3,X5),
      replace_procs_1(X2,X5,X4).
newstart_1([vertex(X1,X2,0,X3,X4)|X5],vertex(X1,X2,0,X3,X4)) :- 
      !.
newstart_1([X1|X2],X3) :- 
      newstart_1(X2,X3).
vars_2(X1,X2,X3) :- 
      var(X1),
      !,
      insertvar_1(X1,X2,X3).
vars_2(X1,X2,X2) :- 
      atomic(X1),
      !.
vars_2(X1,X2,X3) :- 
      nonvar(X1),
      X1=..[X4|X5],
      argvars_1(X5,X2,X3).
remove_any_type_instance_1(X1,X2,X3,X4,[(X5:-true)|X6],X7,X8,X9,X10,X11,X12,X13,X14,X15) :- 
      remove_type_instance_1(X1,X3,X16,X17),
      !,
      X5=..[X7,X16],
      lower_type_1([(X4:-true)|X2],X17,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15).
remove_any_type_instance_1(X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14) :- 
      lower_type_1(X2,X3,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14).
removeclause_1(X1,X2,[(X3:-X2)|X4],X4) :- 
      X1=..[X5,X6],
      X3=..[X7,X8],
      functor(X6,X9,X10),
      functor(X8,X9,X10),
      !.
removeclause_1(X1,true,[(X2:-true)|X3],[(X2:-true)|X3]) :- 
      X2=..[X4,X5],
      basic_type_1(X5),
      X1=..[X6,X7],
      type_check_1(X7,X5),
      !.
removeclause_1(X1,X2,[X3|X4],[X3|X5]) :- 
      removeclause_1(X1,X2,X4,X5).
intersect_body_2(true,true,true,X1,X2,X3,X4,X5,X6,X6,X7) :- 
      !.
intersect_body_2((X1,X2),(X3,X4),(X5,X6),X7,X8,X9,X10,X11,X12,X13,X14) :- 
      !,
      intersect_atom_1(X1,X3,X5,X7,X8,X9,X10,X11,X12,X15,X14),
      intersect_body_2(X2,X4,X6,X7,X8,X9,X10,X11,X15,X13,X14).
intersect_body_2(X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11) :- 
      intersect_atom_1(X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11).
each_intersect_1([],[],X1,X2,X3,X4,X5,X6,X6,X7) :- 
      !.
each_intersect_1([(X1,X2,X3)|X4],[proc(X3/1,X5)|X6],X7,X8,X9,X10,X11,X12,X13,X14) :- 
      def_1(X1,X15,X14),
      def_1(X2,X16,X14),
      X17 is X12+1,
      !,
      lower_type_1(X15,X16,X5,X3,X8,X9,X10,X11,X7,X17,X18,X14),
      each_intersect_1(X4,X6,X7,X8,X9,X10,X11,X18,X13,X14).
giventype_1(X1,(X2,X3),X4) :- 
      X2=..[X4,X5],
      X1==X5,
      !.
giventype_1(X1,(X2,X3),X4) :- 
      !,
      giventype_1(X1,X3,X4).
giventype_1(X1,X2,X3) :- 
      X2=..[X3,X4],
      X1==X4,
      !.
giventype_1(X1,X2,any) :- 
      true.
termtype_1(X1,X2,[proc(X3/1,[(X4:-X5)])|X6],X7,X8) :- 
      functor(X1,X9,X10),
      functor(X11,X9,X10),
      newname_1(X7,X3),
      X4=..[X3,X11],
      canonical_1(X4),
      X12 is X7+1,
      argtypes_1(X1,X2,X11,X5,1,X10,X12,X8,X6).
subtype_pairs_1(true,true,X1,X1,X2) :- 
      !.
subtype_pairs_1((X1,X2),(X3,X4),X5,X6,X7) :- 
      !,
      subtype2_1(X1,X3,X5,X8,X7),
      subtype_pairs_1(X2,X4,X8,X6,X7).
subtype_pairs_1(X1,X2,X3,X4,X5) :- 
      subtype2_1(X1,X2,X3,X4,X5).
body_defs_1([(X1:-X2)|X3],X4,X5,X6,X7) :- 
      bodytypes_1(X2,X4,X5,X6,X7),
      body_defs_1(X3,X4,X5,X6,X7).
body_defs_1([],X1,X2,X3,X4) :- 
      true.
ub_atom_1(X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11) :- 
      X1=..[X12,X13],
      X2=..[X14,X13],
      next_ub_1(X12,X14,X15,X4,X5,X6,X7,X8,X9,X10,X11),
      X3=..[X15,X13].
hclause_1(X1,[[proc(X2/1,[(X3:-X4)])|X5]|X6],X3,X4,X5) :- 
      X3=..[X2,X1],
      !.
hclause_1(X1,[X2|X3],X4,X5,X6) :- 
      hclause_1(X1,X3,X4,X5,X6).
clause_head_functors_1(X1,[],X2,X2) :- 
      true.
clause_head_functors_1(X1,[(X2:-X3)|X4],[head_func(X1,X5/X6)|X7],X8) :- 
      X2=..[X9,X10],
      functor(X10,X5,X6),
      clause_head_functors_1(X1,X4,X7,X8).
normalise_1([],X1,X2,[]) :- 
      true.
normalise_1([(X1:-X2)|X3],X4,X5,[(X1:-X6)|X7]) :- 
      normalise_body_1(X2,X4,X5,X6),
      normalise_1(X3,X4,X5,X7).
replace_proc_1(proc(X1/1,X2),[proc(X1/1,X3)|X4],[proc(X1/1,X2)|X4]) :- 
      !.
replace_proc_1(X1,[X2|X3],[X2|X4]) :- 
      replace_proc_1(X1,X3,X4).
replace_proc_1(X1,[],[X1]) :- 
      true.
insertvar_1(X1,[],[X1]) :- 
      true.
insertvar_1(X1,[X2|X3],[X2|X3]) :- 
      X1==X2,
      !.
insertvar_1(X1,[X2|X3],[X2|X4]) :- 
      insertvar_1(X1,X3,X4).
argvars_1([],X1,X1) :- 
      true.
argvars_1([X1|X2],X3,X4) :- 
      vars_2(X1,X3,X5),
      argvars_1(X2,X5,X4).
remove_type_instance_1(X1,[(X2:-true)|X3],X4,X3) :- 
      X2=..[X5,X4],
      type_check_1(X4,X1),
      !.
remove_type_instance_1(X1,[X2|X3],X4,[X2|X5]) :- 
      remove_type_instance_1(X1,X3,X4,X5).
intersect_atom_1(X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11) :- 
      X1=..[X12,X13],
      X2=..[X14,X13],
      next_intersect_1(X12,X14,X15,X4,X5,X6,X7,X8,X9,X10,X11),
      X3=..[X15,X13].
subtype2_1(X1,X2,X3,X4,X5) :- 
      functor(X1,X6,1),
      functor(X2,X7,1),
      subtype_2(X6,X7,X3,X4,X5).
bodytypes_1((X1,X2),X3,X4,X5,X6) :- 
      !,
      functor(X1,X7,1),
      insertT_1(X7,[X3|X4],X5),
      bodytypes_1(X2,X3,X4,X5,X6).
bodytypes_1(true,X1,X2,X3,X4) :- 
      !.
bodytypes_1(X1,X2,X3,X4,X5) :- 
      functor(X1,X6,1),
      insertT_1(X6,[X2|X3],X4).
next_ub_1(X1,X2,X2,X3,X4,X5,X6,X7,X8,X8,X9) :- 
      subtype_1(X1,X2,X9),
      !.
next_ub_1(X1,X2,X1,X3,X4,X5,X6,X7,X8,X8,X9) :- 
      subtype_1(X2,X1,X9),
      !.
next_ub_1(X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11) :- 
      get_name_1(X1,X2,X3,X4,X5,X6,X7,X8,X9,X10).
normalise_body_1(true,X1,X2,true) :- 
      !.
normalise_body_1((X1,X2),X3,X4,(X5,X6)) :- 
      X1=..[X3,X7],
      !,
      X5=..[X4,X7],
      normalise_body_1(X2,X3,X4,X6).
normalise_body_1((X1,X2),X3,X4,(X1,X5)) :- 
      !,
      normalise_body_1(X2,X3,X4,X5).
normalise_body_1(X1,X2,X3,X4) :- 
      X1=..[X2,X5],
      !,
      X4=..[X3,X5].
normalise_body_1(X1,X2,X3,X1) :- 
      true.
next_intersect_1(X1,X2,X1,X3,X4,X5,X6,X7,X8,X8,X9) :- 
      subtype_1(X1,X2,X9),
      !.
next_intersect_1(X1,X2,X2,X3,X4,X5,X6,X7,X8,X8,X9) :- 
      subtype_1(X2,X1,X9),
      !.
next_intersect_1(X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11) :- 
      get_name_1(X1,X2,X3,X4,X5,X6,X7,X8,X9,X10).
insertT_1(any,X1,X2) :- 
      !.
insertT_1(X1,X2,X3) :- 
      var(X2),
      !,
      memb1_1(X1,X3),
      X2=[X1|X4].
insertT_1(X1,[X1|X2],X3) :- 
      !.
insertT_1(X1,[X2|X3],X4) :- 
      insertT_1(X1,X3,X4).
get_name_1(X1,X2,X3,X4,X5,X6,X7,X8,X9,X10) :- 
      X1@=<X2,
      !,
      insert_pair_1(X1,X2,X3,X4,X5,X6,X7,X8,X9,X10).
get_name_1(X1,X2,X3,X4,X5,X6,X7,X8,X9,X10) :- 
      insert_pair_1(X2,X1,X3,X4,X5,X6,X7,X8,X9,X10).
loopsubtype_1(X1,X2,[(X1,X2)|X3]) :- 
      !.
loopsubtype_1(X1,X2,[X3|X4]) :- 
      loopsubtype_1(X1,X2,X4).
insert_pair_1(X1,X2,X3,X4,X5,X6,X7,X8,X9,X10) :- 
      insert_pair1_1(X1,X2,X3,[(X4,X5,X6)|X7],X8,X9,X10).
insert_pair1_1(X1,X2,X3,X4,X5,X6,X7) :- 
      var(X4),
      !,
      newname_1(X6,X3),
      X7 is X6+1,
      X4=[(X1,X2,X3)|X8],
      memb1_1((X1,X2,X3),X5).
insert_pair1_1(X1,X2,X3,[(X1,X2,X3)|X4],X5,X6,X6) :- 
      !.
insert_pair1_1(X1,X2,X3,[X4|X5],X6,X7,X8) :- 
      insert_pair1_1(X1,X2,X3,X5,X6,X7,X8).

/* time(G):  Gives time to run goal G */

time(G) :-
	statistics(runtime,[_,_]),
	call(G),
	statistics(runtime,[_,T4]),
	T is T4/1000,
	nl,
	write('% Time: '),
	write(T),nl .
	
space(G) :-
	statistics(runtime,[_,_]),
	call(G),
	statistics(runtime,[_,T4]),
	statistics(global_stack,[GS,_]),
	statistics(trail,[Trail,_]),
	statistics(garbage_collection,[GCs,_,_]),
	T is T4/1000,
	nl,
	write('% Time: '), write(T),nl,
	write('% Global stack: '), write(GS),nl,
	write('% Trail: '), write(Trail),nl,
	write('% No. of GCs: '), write(GCs),nl .
	
/* test(G,N) : run goal G N times, and prints runtime  */



test(G,N) :- 
	time(run(G,0,N),Tp),
	time(run(true,0,N),Td),
	Rtp is Tp - Td,
	write(time(Rtp)).

time(G,T) :-
	statistics(runtime,_),
	call(G),
	statistics(runtime,[_,T1]),
	T is T1/1000.0 .

run(G,R,N) :- R < N, call(G), fail .
run(G,R,N) :- R < N, !, Rp1 is R + 1, run(G,Rp1,N) .
run(_,_,_) .


numeric(numeric) :- true.

numexpr(numeric) :- true.
numexpr('X0'+'X1') :- numexpr('X0'),numexpr('X1').
numexpr('X0'-'X1') :- numexpr('X0'),numexpr('X1').
numexpr('X0'/'X1') :- numexpr('X0'),numexpr('X1').
numexpr('X0'*'X1') :- numexpr('X0'),numexpr('X1').
numexpr('X0' mod 'X1') :- numexpr('X0'),numexpr('X1').
numexpr(-'X0') :- numexpr('X0').


