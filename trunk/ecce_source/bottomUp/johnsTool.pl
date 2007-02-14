
:- module(johnsTool, [td/3]).

/************************************************************************/
/*  Copyright (C)1994-99 University of Bristol                          */
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
/*  Copyright (C)1994-99                                                */
/*  JPG/University of Bristol                                           */
/*                                                                      */
/* Regular Approximation Tool                                           */
/*                                                                      */
/* John Gallagher                                                       */
/* Dept. of Computer Science                                            */
/* University of Bristol                                                */
/* Bristol, UK                                                          */
/*                                                                      */
/* john@compsci.bristol.ac.uk                                           */
/*                                                                      */
/* version 6/7/99                                                       */
/*                                                                      */
/************************************************************************/
/*                                                                      */
/* I/O Interface modified by Stefan Gruner, University of Southampton,  */
/* Southampton (GB), November 2001. (now module, td/0 changed to td/3)  */
/*                                                                      */
/************************************************************************/


td(Q,F1,OutFile) :-
	/* new_line,
	write('From File: '),
	read(F1),
	new_line,
	write('Query: '),
	read(Q), */
	magic(F1,query_answer_temp_file,Q),
	sp_consult(query_answer_temp_file),
	program_preds(P),
	time(tp_fix1_adapted(P,F)),
    display_fix(F,OutFile).
	
	
display_fix(Cls,FileName) :-
        /* nl,write('Enter filename: '),
        read(F), */
        reg_filewrite(FileName,Cls).
        
        
tp_fix1_adapted(P,F) :-
	strong_connected_components(P,G),% compute the dependency ordering
	nl,
	tp_fix_groups(G,[],F,10,K),
	/* show_useless_clauses(P,F,K,_), */
	precision(P,F).
	
	
:- ['/Documents/Programming/tools/spsys/APE/sics_sp.pl'].