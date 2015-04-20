:- module(_,_,[assertions, make, functions]).

:- use_module(library('make/system_extra')).
:- use_module(library(terms),[atom_concat/2]).

main := paper.

dvi <= tex :: File :-
	system(~atom_concat(['latex ',File,'.tex'])).

ps  <= dvi :: File :-
	system(~atom_concat(['dvips -o ',File,'.ps ',File,'.dvi'])).

pdf  <= ps :: File :-
	system(~atom_concat(['ps2pdf ',File,'.ps'])).

view <- ~atom_concat([~main,'.ps']) :-
 	system(~atom_concat(['ghostview ',~main,'.ps'])).

target_comment(view) :- display('Visualization of formatted paper.\n').

clean <- :-
	delete_files(~ls('*aux|*log|*~|*.asr|*.itf|*.po')).

target_comment(clean) :- display('Cleanup of temporary files.\n').

realclean <- clean :-
	delete_files(~ls('*dvi|*ps|*pdf')).

target_comment(realclean) :- display('Cleanup of all generated files.\n').

%% -------------------------------------------------------------------------

dependency_comment(SSuffix,TSuffix,FileBase) :- 
	display(~atom_concat(['Generation of ',FileBase,TSuffix,
                              ' from ',FileBase,SSuffix,'.\n'])).
