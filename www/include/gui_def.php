<?php

def_button('pl_out','true','','fkt_svg_out');
def_button('svg_out','true','','fkt_svg_out');
def_button('load_file','Load','Upload a Prolog-File','fkt_load_file');
def_button('save_file','Save','Save the Prolog-File','fkt_save_file');
def_button('pe','Specialise','Partially evaluate the program for a specified goal','fkt_pe');
def_button('slice','Slice','Slice program for goal','fkt_slice');
def_button('msv','MSV','Most Specific Version bottom-up propagation','fkt_msv');
def_button('raf','RAF','Redundant Argument Filtering (RAF)','fkt_raf');
def_button('far','FAR','Reverse Redundant Argument Filtering (FAR)','fkt_far');
def_button('save_xce','Save','Save result of Evaluation','fkt_save_xce');
def_button('swap','Move to Source','Move to Source','fkt_swap');
def_button('dot','Specialisation Tree','Show Specialisation Tree','fkt_dot');
def_button('pre_load','Load','Load a working example','fkt_pre_load');
def_button('rul','RUL','Regular Unary Logic program analysis (Goal-Directed) by John Gallagher','fkt_rul');
def_button('rul_bup','RUL-BUP','Regular Unary Logic bottom-up analysis by John Gallagher','fkt_rul_bup');
def_button('nfta','NFTA','Non-Deterministic Finite Tree Automaton (Goal-Directed) by John Gallagher','fkt_nfta');
def_button('nfta_bup','NFTA-BUP','Non-Deterministic Finite Tree Automaton (Bottom-Up) by John Gallagher','fkt_nfta_bup');
def_button('ciao_optimise','CiaoPP Optimise','Compute CiaoPP Optimisation','fkt_ciao_optimise');
def_button('ciao_analyse','CiaoPP Analyse','Use CiaoPP Analyser','fkt_ciao_analyse');




def_cb('radio','config','config_default','Default Conjunctive',TRUE);
def_cb('radio','config','fast','Conjunctive Fast',FALSE);
def_cb('radio','config','classic','Classic',FALSE);
def_cb('radio','config','classic-fast','Classic Fast',FALSE);
def_cb('radio','config','mixtus','Mixtus Style',FALSE);
def_cb('radio','config','minimal','Minimal',FALSE);
def_cb('radio','config','term','Termination',FALSE);

def_cb('radio','pp','pp_max','Max',FALSE);
def_cb('radio','pp','pp_default','Default',TRUE);
def_cb('radio','pp','pp_off','Off',FALSE);



$GLOBALS['prolog_examples']=array(
'ex01'=>array('des'=>'vanilla_list.pl',          'file'=>'benchmarks/vanilla_list.pl',        'goal'=>'test(R)'   ),
'ex02'=>array('des'=>'match.pl',                 'file'=>'benchmarks/match.pl',               'goal'=>'test(S)'   ),
'ex03'=>array('des'=>'even_odd.pl',              'file'=>'benchmarks/even_odd.pl',            'goal'=>'eo(X)'   ),
'ex04'=>array('des'=>'depth.pl',                 'file'=>'benchmarks/depth.pl',		      'goal'=>'test2(X,D)'   ),
'ex05'=>array('des'=>'regular_expression.pl',    'file'=>'benchmarks/regular_expression.pl',  'goal'=>'test(S)'   ),
'ex06'=>array('des'=>'matrix.pl',		 'file'=>'benchmarks/matrix.pl',	      'goal'=>'inv(X)'   ),
'ex07'=>array('des'=>'maxlen.pl',                'file'=>'benchmarks/maxlen.pl',	      'goal'=>'maxlen(Ls,M,L)'   ),
'ex08'=>array('des'=>'groundunify.pl',           'file'=>'benchmarks/groundunify.pl',         'goal'=>'test(X)'   ),
'ex09'=>array('des'=>'map.pl',                   'file'=>'benchmarks/map.pl',		       'goal'=>'test(X)'   ),
'ex10'=>array('des'=>'rotateprune.pl',           'file'=>'benchmarks/rotateprune.pl',          'goal'=>'test(X)'   ),
'ex11'=>array('des'=>'relative.pl',              'file'=>'benchmarks/relative.pl',          'goal'=>'test(X)'  ),
'ex12'=>array('des'=>'advisor.pl',		 'file'=>'benchmarks/advisor.pl',          'goal'=>'test(X)'  ),
'ex13'=>array('des'=>'ctl_specialised_petri.pl', 'file'=>'benchmarks/ctl_specialised_petri.pl',          'goal'=>'sat(X)'  ),
'ex14'=>array('des'=>'ctl_trace.pl',		 'file'=>'benchmarks/ctl_trace.pl',          'goal'=>'test(X)'  ),
'ex15'=>array('des'=>'liftsolve.pl',		 'file'=>'benchmarks/liftsolve.pl',          'goal'=>'test(X)'  ),
'ex16'=>array('des'=>'transpose.pl',		 'file'=>'benchmarks/transpose.pl',          'goal'=>'test(X)'  ),
'ex17'=>array('des'=>'member_from_delete.pl',	 'file'=>'benchmarks/member_from_delete.pl',     'goal'=>'member(X,L)'   ),
'ex18'=>array('des'=>'power.pl',		 'file'=>'benchmarks/power.pl',                  'goal'=>'square(X,Res)' ),
'ex19'=>array('des'=>'append.pl',		 'file'=>'benchmarks/append.pl',                  'goal'=>'append([a,b,c,d|X],Y,Res)' ),
'ex20'=>array('des'=>'reverse.pl',		 'file'=>'benchmarks/reverse.pl',                  'goal'=>'rev([a,b,c,d|X],Res)' ),
'ex21'=>array('des'=>'prop_int.pl',		 'file'=>'benchmarks/prop_int.pl',                  'goal'=>'test(X)' ),
)

?>