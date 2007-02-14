:- module(dot_generator,
	[
	    unfold_generate_dot_file/1,
	    unfold_generate_dot_file/0,
	    gt_print_node_ids/1,
	    gt_dot_display_localtrees/1,
	    gt_dot_display_instances/1,
	    set_gt_dot_display_instances/1,
	    set_gt_dot_display_localtrees/1,
	    dot_generate_all_pdf/0,
	    reset_dot_animation_counter/0
	]).

:- use_package( .(ecce_no_rt) ).


/* ['dot_generator.pro'],print_chtree_for_dot(NodeID) . */

:- set_prolog_flag(multi_arity_warnings,off).
:- set_prolog_flag(discontiguous_warnings,off).
:- set_prolog_flag(single_var_warnings,off).

:- use_module(dynpreds).

:- use_module(library(dec10_io)).
:- use_module(library(system)).
:- use_module(library(lists)).
   
:- use_module(bimtools).
:- use_module(global_tree).
:- use_module(code_generator).
:- use_module(calc_chtree).

:- use_module('more_specific/more_specific').


% --- DTM: if sicstus
:- op( 1000 , fx , data ).


:- data dot_animation_counter/1.
dot_animation_counter(0).

reset_dot_animation_counter :-
  retractall(dot_animation_counter(_)),
  assert(dot_animation_counter(0)).



:- data auto_generate_dot_files/1.
auto_generate_dot_files(no).

auto_generate_dot_files :- auto_generate_dot_files(yes).

set_auto_generate_dot_files(NewVal) :-
   retract(auto_generate_dot_files(_)),
   assert(auto_generate_dot_files(NewVal)).

/* ?- assert(auto_generate_dot_files). */


:- data gt_print_node_ids/1.
gt_print_node_ids(no).
set_gt_print_node_ids(NewVal) :-
   retract(gt_print_node_ids(_)),
   assert(gt_print_node_ids(NewVal)).
   
   
:- data gt_dot_display_instances/1.
gt_dot_display_instances(yes).
set_gt_dot_display_instances(NewVal) :-
   retract(gt_dot_display_instances(_)),
   assert(gt_dot_display_instances(NewVal)).
   
   
:- data gt_dot_display_localtrees/1.
gt_dot_display_localtrees(yes).
set_gt_dot_display_localtrees(NewVal) :-
   retract(gt_dot_display_localtrees(_)),
   assert(gt_dot_display_localtrees(NewVal)).
   
   
:- data gt_dot_put_code_nodes_same_rank/1.
gt_dot_put_code_nodes_same_rank(yes).
set_gt_dot_put_code_nodes_same_rank(NewVal) :-
   retract(gt_dot_put_code_nodes_same_rank(_)),
   assert(gt_dot_put_code_nodes_same_rank(NewVal)).
   
:- data gt_adorn_edges/1.
gt_adorn_edges(yes). /* turn off for import into Omni Graffle */
set_gt_adorn_edges(NewVal) :-
   retract(gt_adorn_edges(_)),
   assert(gt_adorn_edges(NewVal)).



unfold_generate_dot_file :-
   auto_generate_dot_files,!,
   retract(dot_animation_counter(X)),
   X1 is X+1,
   assert(dot_animation_counter(X1)),
   string_concatenate('$ECCE_SYSTEM/tmp/local_tree_',X,F1),
   string_concatenate(F1,'.dot',F),!,
   print(gen(F)),nl,
   unfold_generate_dot_file(F),
   string_concatenate(F1,'.ps',PSF),
   string_concatenate(' -o ',PSF,Cmd0),
   string_concatenate(F,Cmd0,Cmd1),
   string_concatenate('dot -Tps ',Cmd1,Cmd),
   print(exec(Cmd)),nl,
   system(Cmd),
   %string_concatenate('gv ',PSF,GVCmd),
   %system(GVCmd),
   string_concatenate(F1,'.pdf',PDF),
   string_concatenate(' ',PDF,PDF1),
   string_concatenate(PSF,PDF1,PDFArgs),
   string_concatenate('ps2pdfwr ',PDFArgs,PdfCmd),
   print(exec(PdfCmd)),nl,
   system(PdfCmd).
unfold_generate_dot_file.


dot_generate_all_pdf :- 
   auto_generate_dot_files,!,
   print('Generating a single pdf file'),nl,
   tell('$ECCE_SYSTEM/tmp/all.tex'),
   print('\\documentclass[a4paper]{article}'),nl,
   print('\\usepackage{pdfpages}'),nl,nl,
   print('\\begin{document}'),nl,nl,
   dot_animation_counter(Cnt),
   print_pdf_includes(0,Cnt),
   print('\\end{document} '),nl,
   told.
   %system('pdflatex $ECCE_SOURCE/../tmp/all.tex').
dot_generate_all_pdf.

print_pdf_includes(X,Y) :- X < Y,!,
   print('\\includepdf[pages=-]{local_tree_'),print(X),
   print('}'),nl,
   X1 is X+1,
   print_pdf_includes(X1,Y).
print_pdf_includes(_,_).


unfold_generate_dot_file(F) :-
   tell(F),
   (print_chtree_for_dot(_) -> true ; true),
   told.

print_chtree_for_dot(NodeID) :-
    print('digraph local_trees {'),nl,
   % print('graph [orientation=landscape, page="8.5, 11",ratio=fill,size="7.5,10"];'),nl,
    print('graph [page="8.5, 11",ratio=fill,size="7.5,10"];'),nl,
	gt_node(NodeID),
	dot_display_node(NodeID),
	gt_node_goal(NodeID,Goal), 
	gt_node_chtree(NodeID,Chtree),
	print_root_dot_arc(NodeID,Goal,NodeID),
	
	gt_dot_display_localtrees(yes),
	code_has_to_be_generated_for_node(NodeID),
	print_chtree_for_dot(Chtree,Goal,NodeID),
	fail.
print_chtree_for_dot(NodeID) :-
	gt_node(NodeID),
	dot_display_node(NodeID),
	gt_node_descends_from(NodeID,DescID,_LeafLocalID),
	(gt_node_instance_of(DescID,_)
	  -> (gt_dot_display_instances(yes) ;
	      gt_node_descends_from(DescID,_,_)
	     )
	  ; true
	),
	print(DescID), print(' -> '),
	print(NodeID), 
	((_LeafLocalID = chpos(abstracted,_))
	  -> print_edge_info(dotted,red,gen)
	  ;  print_edge_info(solid,blue,none)  /* none */
	),
	fail.
print_chtree_for_dot(NodeID) :-
    gt_dot_display_instances(yes),
	gt_node(NodeID),
	((dot_display_node(NodeID),
	  gt_node_instance_of(NodeID,GeneralID))
	  ->
	     (print(NodeID), print(' -> '),
	      print(GeneralID), 
	      print_edge_info(dashed,lightgrey,inst),
	      nl)
	),
	fail.
print_chtree_for_dot(_) :-
    print_same_ranks,
    print('}'),nl.

print_root_dot_arc(ClauseNr,NewGoal,NewID) :-
   (var(NewID) -> gennum(NewID) ; true),
   print(NewID), 
   print(' ['),
   print_node_shape(NewID),
   print(' label="'),
   dot_print_goal(NewID,NewGoal),print('"];'),
   nl.
  /*,
   print(root), print('->'), print(NewID),
   print(' [style = dotted, label="'), print(ClauseNr), print('"];'),nl. */
   

print_edge_info(Style,Color,Label) :- gt_adorn_edges(yes),!,
    print('[style = '), print(Style),
    ((Label\=none) -> (print(', label="'), print(Label),  print('"')) ; true),
    print(', color='),print(Color),
    print('];'),nl.
print_edge_info(_Style,_Color,_Label) :- print(';'),nl.
   
dot_display_node(NodeID) :- generate_dot_nodes_for_leaves(yes),gt_node(NodeID).
dot_display_node(NodeID) :- 
    generate_dot_nodes_for_leaves(no),
    (code_has_to_be_generated_for_node(NodeID) ;
     get_gt_goal_to_pe(NodeID,_) ;
     node_is_abstracted(NodeID)
    ).
    
print_node_shape(NodeID) :-
   node_is_abstracted(NodeID),!,
   print('shape=box, style=filled, color=lightgrey,').
print_node_shape(NodeID) :-
   code_has_to_be_generated_for_node(NodeID),
   node_is_an_abstraction(NodeID),!,
   print('shape=box, style=filled, color=".7 .3 1.0",').
print_node_shape(NodeID) :-
   code_has_to_be_generated_for_node(NodeID),
   get_filtered_version(_,NodeID,_),!,
   print('shape=box, style=filled, color=".2 .9 .9",').
print_node_shape(NodeID) :-
   code_has_to_be_generated_for_node(NodeID),!,
   print('shape=box, style=filled, color=".3 .95 .95",').
print_node_shape(NodeID) :-
   gt_node_instance_of(NodeID,_),!,
   print('shape=ellipse, style=filled, color=lightgrey,').
print_node_shape(_NodeID) :-
   print('shape=ellipse, style=filled, color=yellow,').
   
print_same_ranks :-  gt_dot_put_code_nodes_same_rank(no),!.
print_same_ranks :-
  print('{ rank=same; '),
  gt_node(NodeID),
	(code_has_to_be_generated_for_node(NodeID) ; get_gt_goal_to_pe(NodeID,_)),
  print(NodeID), print('; '),
  fail.
print_same_ranks :- print(' }'),nl.  

dot_print_goal(ID,X) :- 
    ((gt_print_node_ids(yes),gt_node(ID))
     -> (print(ID), print(':'), print('\\'),print('n'))
     ;  (true)
    ),
    ((gt_node(ID),code_has_to_be_generated_for_node(ID),
      get_filtered_version(X,ID,FGoal))
      -> (copy_term((X,FGoal),(X2,FG2)), numbervars((X2,FG2),0,_),
          ((FG2 = [FG3]) -> print(FG3) ; print(FG2)),
          print('  == '), print('\\'),print('n'),
          dot_print_goal2(X2) )
      ;  (copy_term(X,X2), numbervars(X2,0,_), dot_print_goal2(X2))
    ).

dot_print_goal2([H]) :- !,print(H).
dot_print_goal2([H,H2|T]) :- !, print(H), print(',\\n'),dot_print_goal2([H2|T]).
dot_print_goal2(X) :- print(X).

/* ---------------------- */
/* print_chtree_for_dot/2 */
/* ---------------------- */

:- include( multi_meta ).


print_chtree_for_dot(Chtree,Goal,ID) :-
    print_chtree_for_dot2(Chtree,Goal,ID).
/* subgraphs do not seem to work:    
    print('   subgraph sub'),print(ID), print(' {'),nl,
    print('     style = filled;'),nl,
    print('     color = lightgrey;'),nl,
    print('     label = "local";'),nl,
    print_chtree_for_dot2(Chtree,Goal,ID),
    fail.
print_chtree_for_dot(_Chtree,_Goal,_ID) :-
    print('   }'),nl.
    */

pre_condition(print_chtree_for_dot2(Chtree,Goal,ID)) :-
	term_is_of_type(Chtree,chtree),
	term_is_of_type(Goal,goal),
	ground(ID).
post_condition(print_chtree_for_dot2(_Chtree,_Goal,_)).

print_chtree_for_dot2(empty,_Goal,_) :- fail.
print_chtree_for_dot2(success,_Goal,_).
print_chtree_for_dot2(stop,Goal,_) :-
	pp_cll(more_specific_transformation(Goal)).
print_chtree_for_dot2(select(SelLitNr,Chpaths),Goal,ID) :-
	pp_cll(more_specific_transformation(Goal)),
	pp_mnf(split_list(Goal,SelLitNr,Left,SelCall,Right)),
	peel_off_calls(SelCall,Sel),
	member(match(ClauseNr,SubTree),Chpaths),
	(claus(ClauseNr,Sel,CBody) -> (true)
		; (print('### Error: clause not matching in print_chtree_for_dot2/4'),nl,
		   print('###  ClauseNr:'),print(ClauseNr),nl,
		   print('###  SelAtom: '),print(Sel),nl,
		   print('###  Goal: '),print(Goal),nl,fail)
	),
	pp_mnf(append(CBody,Right,IntGoal)),
	pp_mnf(append(Left,IntGoal,NewGoal)),
	print_dot_arc(Goal,ID,unf(SelLitNr,ClauseNr),NewGoal,NewID),
	pp_cll(print_chtree_for_dot2(SubTree,NewGoal,NewID)).
print_chtree_for_dot2(built_in_eval(NrOfBI,BI,SubTree),Goal,ID) :-
	pp_cll(more_specific_transformation(Goal)),
	pp_mnf(split_list(Goal,NrOfBI,Left,Sel,Right)),
	(get_predicate_through_calls(Sel,BI) -> true
	 ;  (print('### Warning: illegal built-in in print_chtree_for_dot2/4'),nl,
	     print('###  '),print(Sel), print(' is not '),
	     print(BI),nl
	    )
	),
	((is_callable_built_in_literal(Sel))
		-> (call_built_in(Sel))
		; (built_in_generates_bindings(Sel)
		   -> (print('### Error: illegal built-in in print_chtree_for_dot2/4'),nl,
		       print('###  '),print(Sel),
		       print(': generates bindings and is not callable'),nl
		      )
		    ;  (true)
		  )
	),
	pp_mnf(append(Left,Right,NewGoal)),
	print_dot_arc(Goal,ID,bi(NrOfBI),NewGoal,NewID),
	pp_cll(print_chtree_for_dot2(SubTree,NewGoal,NewID)).
print_chtree_for_dot2(remove(SelLitNr,_Predicate,SubTree),Goal,ID) :-
	pp_cll(more_specific_transformation(Goal)),
	pp_mnf(split_list(Goal,SelLitNr,Left,_Sel,Right)),
	pp_mnf(append(Left,Right,NewGoal)),
	print_dot_arc(Goal,ID,remove(SelLitNr),NewGoal,NewID),
	pp_cll(print_chtree_for_dot2(SubTree,NewGoal,NewID)).
	

print_dot_arc(_Goal,ID,ClauseNr,NewGoal,NewID) :-
   (var(NewID) -> gennum(NewID) ; true),
   print(NewID), print(' [shape=box, color=green, label="'),
   dot_print_goal(NewID,NewGoal),print('"];'),
   nl,
   print(ID), print('->'), print(NewID),
   print_edge_info(solid,green,ClauseNr).
