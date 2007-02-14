%:- module(cli_polyhedra,[main_sicstus/0, main/1]).
/* File: cli_polyhedra.pl */
/* Created: 01/02/2007 by Michael Leuschel */


:- use_module(analyse).
:- use_module(library(lists)).

% CLI PART

main_sicstus :-
   prolog_flag(argv,ArgV),
   main(ArgV).
 
main(ArgV) :-
   get_options(ArgV,Recogn,Remaining),
   Remaining = [FILE|_],!,
   (member(help,Recogn) -> print_help ; true),
   (member(debug_mode,Recogn) -> turn_debug_on ; true),
   (member(term_norm,Recogn) -> set_convex_norm(term) ; true),
   (member(both_norm,Recogn) -> set_convex_norm(both) ; true),
   print('% Performing convex hull analysis...'),nl,
   go(FILE).
main(_) :- print_help.


print_help :-
   print('Convex Hull Analyser'),nl,
   print(' code by:  Florence Benoy, Andy King and Fred Mesnard '),nl,
   print('           (small additions by Michael Leuschel)'),nl,
   print('USAGE: '),nl,
   print('convex [OPTIONS] FILE'),nl,
   print('        -d   debug mode'),nl,
%   print('        -t   use term_size norm (default list length)'),nl,
%   print('        -b   use both term size and list length norm'),nl,
   nl.


get_options([],Rec,Rem) :- !,Rec=[],Rem=[].
get_options(Inputs,RecognisedOptions,RemOptions) :-
   (recognise_option(Inputs,Flag,RemInputs)
     -> (RecognisedOptions = [Flag|RecO2], RemO2 = RemOptions)
     ;  (Inputs = [H|RemInputs], RemOptions = [H|RemO2], RecO2 = RecognisedOptions)
   ),
   get_options(RemInputs,RecO2,RemO2).

recognise_option(Inputs,Flag,RemInputs) :-
   recognised_option(Heads,Flag),
   append(Heads,RemInputs,Inputs).
   
%recognised_option(['-d'],debug_mode).
%recognised_option(['-t'],term_norm).
%recognised_option(['-b'],both_norm).
recognised_option(['-h'],help).
recognised_option(['--help'],help).



runtime_entry(start) :- main_sicstus.