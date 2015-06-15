%:- module(ecce_tcltk ,
%	[
%	    main/0,
%	    tcltk_initialise/0,	    
%	    tcltk_turn_type_checking_off/0,
%	    tcltk_open/1,
%	    tcltk_set_specfile/1,
%	    tcltk_specialise/1,
%	    tcltk_turn_rul_on/0,
%	    tcltk_turn_rul_bup_on/0
%	]).

/* ---------------------------------- */
/* (c) Michael Leuschel, Jan/Feb 2001 */
/* ---------------------------------- */

/* load all necessary modules */
:- use_module(library(tcltk)).
:- use_module(library(random)).
:- use_module(library(system),[]).

:- if(current_prolog_flag(version_data,sicstus(3,_,_,_,_))).
:- initialization(system:environ('ECCE_SOURCE',_R)).
:- initialization(system:working_directory(_,_R)). % is now current_directory in SICS 4 in file_systems
:- endif.

portray_message(informational, _).
runtime_entry(start) :- go.
main :- go.

save :- save_program('~/git_root/ecce/ecce_source/ecce.sav').
%:- ensure_consulted('ecce_main.pl').
% MV: I am bypassing ecce_sicstus.pl!!!
% MAL: this does not work !!!!
:- ensure_loaded('ecce_sicstus.pl').


/* main program 
 * ============
 */


go :-
    initialise_parameters,
    tk_new([name('Ecce')], X),
    tcl_eval(X, 'source ecce_tcltk.tcl', _),
    tk_main_loop,
    tcl_delete(X).

/* -------------------------------------------------------------------- */

/* for Tcl/Tk Interface */


tcltk_initialise :-
   set_user_expert(yes).
   /*tcltk_turn_type_checking_off.*/

tcltk_turn_type_checking_off :-
   ecce_reconsult('bimtools/prepost.nocheck.pl').
   
tcltk_open(Filename) :-
   clear_database,
   read_in_file(Filename).
   
tcltk_set_specfile(FileName) :-
  set_output_to_file(FileName).

tcltk_specialise(Goal) :-
   copy_term(Goal,G),
   pe(G).

tcltk_turn_rul_on :-
   set_user_expert(yes),
   set_standard_config(110), /* minimal post-processing */
   set_standard_config(114). /* enable rul */
   
   
tcltk_turn_rul_bup_on :-
   tcltk_turn_rul_on,
   ['$ECCE_SOURCE/bottomUp/adhoc_bup'].

/* :- nl,print('Type go.<return> to start the animator'),nl,nl. */


:- initialization(nl).
:- initialization(print('Starting ECCE')).
:- initialization(nl).
%:- initialization(debug).
:- initialization(go).

