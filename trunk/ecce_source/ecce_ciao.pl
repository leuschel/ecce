%:- module(ecce_ciao,[main/1,ecce/0]).
:- module( _ , _ , [default] ). /* mv */

/* ================================= */
/*   E C C E       for Ciao Prolog   */
/* ================================= */



/* --------------------------------------------- */
/* (C) COPYRIGHT MICHAEL LEUSCHEL 1995,96,97,98  */
/*     [Ported to Ciao by Mauricio Varea, 2003]  */
/* --------------------------------------------- */


/* load this file to start up the ECCE system in Ciao Prolog */


% ecce_source_directory('$ECCE_SOURCE/').
% ecce_benchmark_directory('$ECCE_BENCHMARKS/').


:- set_prolog_flag(multi_arity_warnings,off).
:- set_prolog_flag(discontiguous_warnings,off).
:- set_prolog_flag(single_var_warnings,off).

:- use_module(library(lists)).
:- use_module(library(compiler)).
:- use_module(library(dec10_io)).
:- use_module(library(prolog_sys)).
:- use_module(library(terms_check)).
:- use_module(library(dynamic)).
:- use_module(library(system)).


read_from_chars(String, Term,Exc) :-
        mktemp('/tmp/readatomXXXXXX',TmpFile),
        open(TmpFile, write, TmpOut),
        display(TmpOut, String),
        display(TmpOut, ' .\n'),
        close(TmpOut),
        open(TmpFile, read, TmpIn),
        catch((read(TmpIn, Term),Exc=none),Exc,true),
        close(TmpIn).
        %print(read_from_chars(String,Term)),nl.


convert_cli_into_atom(CLIGOAL,Atom) :- read_from_chars(CLIGOAL,Atom,Exc),
  (Exc=none -> true
    ; (format(user_error,"*** Error in command-line goal: '~w'.~n*** ~w.~n",[CLIGOAL,Exc]),
       halt(1))).
  

%JMGP :- include('ecce_main.pl').
:- include('ecce_main.pl').
%:- ensure_loaded(ecce_main).
