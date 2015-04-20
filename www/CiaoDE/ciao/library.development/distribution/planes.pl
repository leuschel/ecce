% Example: concurrency and parallelism operators

:- use_module(library(unix)).

:- include(library('distribution/ciao_tcltk')).
:- include(ciaod_kludge).

main :- green.

% Some sample queries.
green    :-     plane(green).
red      :-     plane(red).
yellow   :-     plane(yellow).
cyan     :-     plane(cyan).
seq      :-	green, red.
par      :-	green & red.
urpar    :-	green &> H,  red, H <& .
lpar     :-	green &   red &   yellow &   cyan.
thr      :-	green &&, red.
at(X)    :-	green @ X .
synchro  :-     sync_plane(green, ok(X)) &&, sync_plane(red, X).

% Code: 
plane(Id) :- 
	create_server(Id,PortId),
	canvas_init(PortId),
	do_plane(Id,PortId),
	close_tcltk_server(PortId).

sync_plane(Id, X) :-
	create_server(Id,PortId),
	canvas_init(PortId),
        sync_do_plane(Id, PortId, X),
        close_tcltk_server(PortId).

do_plane(C,PortId) :-
	name(C,Cs),
	create_plane(PortId,Cs),
	slide_plane(462,PortId,Cs),
	pause(1).

sync_do_plane(C,PortId,X) :-
	name(C,Cs),
	create_plane(PortId,Cs),
	sync_slide_plane(462,PortId,Cs,X).

slide_plane(0,_,_).
slide_plane(N,PortId,Id) :-
	N > 0,
	move_plane(PortId,Id),
        plane_loop(2000),
	N1 is N-1,
	slide_plane(N1,PortId,Id).

sync_slide_plane(N, PortId,Id, X) :-
        ask(X = ok(Y)),
        sync_slide_plane_(N, PortId,Id,Y).

sync_slide_plane_(0,_,_,X) :-
        X = ok(_).
sync_slide_plane_(N,PortId,Id,X) :-
	N > 0,
        X = ok(Y),
	move_plane(PortId,Id),
	move_plane(PortId,Id),
	move_plane(PortId,Id),
	N1 is N-3,
	sync_slide_plane(N1,PortId,Id,Y).

% Graphic routines

canvas_init(PortId) :-
	port_send(PortId,"canvas .c1"),
	port_send(PortId,"pack .c1 -fill both -expand yes"),
	port_send(PortId,".c1 configure -background bisque").
%        port_send(PortId, "tkwait visibility .c1").

create_plane(PortId,Col) :-
	string_append(
          [".c1 create bitmap 55 64 -bitmap @plane.xbm -foreground ", Col,
           " -background bisque -tag ", Col], Command1),
	port_send(PortId, Command1),
	worker(W),
	current_host(H),
	string_append(
          [".c1 create text 50 64 ",
           "-font -adobe-courier-bold-r-*-*-18-*-*-*-*-*-*-* ",
           "-text ", W, "@", H, " -tag ", Col], Command2),
	port_send(PortId, Command2),
        string_append([".c1 move ", Col, " 1 0"], Command3),
	port_send(PortId,Command3),
        port_send(PortId, "update"),
%        port_send(PortId, "update idletasks"),
        pause(1).

move_plane(PortId,Id) :-
	string_append([".c1 move ", Id, " 1 0"], Command),
	port_send(PortId,Command).

create_server(red,PortId) :-
	!,
	create_tcltk_server(PortId,"600x128-0-180").
create_server(yellow,PortId) :-
	!,
	create_tcltk_server(PortId,"600x128-0-334").
create_server(cyan,PortId) :-
	!,
	create_tcltk_server(PortId,"600x128-0-488").
create_server(_,PortId) :- 
	create_tcltk_server(PortId,"600x128-0-26").

append([],X,X).
append([X|Y],Z,[X|W]) :- append(Y,Z,W).

% Simple delay loop
plane_loop(0).
plane_loop(N) :-
	N > 0,
	N1 is N-1,
	plane_loop(N1).
