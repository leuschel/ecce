
:- module(geometry1,[main/0],[objects]).

:- use_class(library('tcltk/examples/oval_class')).
:- use_class(library('tcltk/examples/canvas_class')).
:- use_class(library('tcltk/examples/poly_class')).

main :-
	Left_eye new oval_class((100,50),50,25),
	Right_eye new oval_class((190,50),50,25),

	Left_eye:set_border_width(3),
	Right_eye:set_border_width(3),
	Left_eye:set_bg_color(blue),
	Right_eye:set_bg_color(blue),

	Lips new poly_class([(100,150),(120,170),(170,170),(190,150)]),
	
	Face1 new canvas_class([Left_eye,Right_eye,Lips]),
	Face1:show,
	hit_enter,
	destroy Face1,
	destroy Left_eye,
	destroy Right_eye.

hit_enter :-
	display('Hit ENTER to continue...'),
	nl,
	get_code(_).
