:- module(_,_,_).

:-use_module(dirs_manual).

:-load_compilation_module(dirs_manual).

:-use_module(lpdoclib(autodoc)).

main :-
	display(hola).
