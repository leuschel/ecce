:- module( prueba_vrml,
	[test/1]).

:- include(library(assertions)).
:- include(library(basicmodes)).
:- include(library(types)).

:- use_module(library(lists), [append/3]).
:- use_module(library(system)).
:- use_module(vrml_communication).
:- use_module('/home/clip/Systems/ciao/library.development/vrml/FDvis/curve').
:- use_module('/home/goran/PL/CODE/i_o').
:- use_module('/home/clip/Systems/ciao/library.development/vrml/vrml').
:- use_module('/home/clip/Systems/ciao/library.development/vrml/generator',[generator/2]).




test(signal) :-
	read_file('/home/clip/Systems/ciao/library.development/vrml/Test/signal0.wrl',VRML),
	vrml_connection(Viewer),
	write(Viewer),nl,
	view_code(Viewer, VRML, _),
	read(_).




test(change) :-
	curve(5,10,Terms1),
	terms_to_vrml(Terms1, VRML1),
	!,
	vrml_connection(Viewer),
	view_code(Viewer, VRML1, Viewer1),
	curve(2,20,Terms2),
	read(_),
	terms_to_vrml(Terms2, VRML2),
	view_code(Viewer1, VRML2, Viewer2),
	curve(4,20,Terms3),
	terms_to_vrml(Terms3, VRML3),
	curve(5,20,Terms4),
	terms_to_vrml(Terms4, VRML4),
	read(_),
	view_code(Viewer2, VRML3,Viewer4),
	get_viewer(Viewer4,0,X0),
	get_viewer(Viewer4,1,X1),
	get_viewer(Viewer4,2,X2),
	read(_),
	change_code(Viewer4, X0, VRML4),
	read(_),
	change_code(Viewer4, X1, VRML4),
	read(_),
	change_code(Viewer4, X2, VRML4),
	change_code(Viewer4, X0, VRML1),
	change_code(Viewer4, X1, VRML1).

test(rotate) :-
	curve(5,25,Terms1),
	terms_to_vrml(Terms1, VRML1),
	vrml_connection(Viewer),
	write(111),nl,
	view_code(Viewer, VRML1, Viewer1),
	curve(4,25,Terms2),
	terms_to_vrml(Terms2, VRML2),
	curve(3,25,Terms3),
	terms_to_vrml(Terms3, VRML3),
	read(_),
	get_viewer(Viewer1, 0, X0),
	write(change_code),nl,
	change_code(Viewer1, X0, VRML2),
	write(change_code),nl,
        read(_),
	change_code(Viewer1, X0, VRML3),
	write(rotate),nl,
        read(_),
	rotate_code(Viewer1, X0, 0),
	write(rotate),nl,
        read(_),
	rotate_code(Viewer1, X0, 1),
	write(rotate),nl,
	read(_),
	rotate_code(Viewer1, X0, 2).

test(rot) :-
	curve(50,5,Terms1),
	curve(4,25,Terms2),
	curve(3,5,Terms3),
	terms_to_vrml(Terms1, VRML1),
	terms_to_vrml(Terms2, VRML2),
	terms_to_vrml(Terms3, VRML3),
%%	write(write_port_number),nl,
%%	read(P),
	vrml_connection(Viewer),
	write(first_code),nl,
	view_code(Viewer, VRML1, Viewer1),
	get_viewer(Viewer1, 0, X0),
	write(enter_to_change_code_in_viewer_),
	write(X0),nl,
	read(_),
	change_code(Viewer1, X0, VRML2),
	read(_),
        write(change_code_again),nl,
       	change_code(Viewer1, X0, VRML3),
	write(enter_to_rotate_back_one),nl,
        read(_),
	rotate_back(Viewer1, X0),
	write(enter_to_rotate_back_one),nl,
        read(_),
	rotate_back(Viewer1, X0),
	write(enter_to_rotate_back_one),nl,
	read(_),
	rotate_back(Viewer1, X0),
	write(enter_to_rotate_forward_one),nl,
        read(_),
	rotate_forth(Viewer1, X0),
        write(enter_to_rotate_forward_one),nl,
        read(_),
	rotate_forth(Viewer1, X0).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%code_analasys(Code, Result).
