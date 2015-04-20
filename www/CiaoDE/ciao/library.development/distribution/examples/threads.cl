% Example: concurrency and parallelism operators

:- module(threads,_,ciao(h)).

% Some sample queries.
green    :-     bomb(green).
red      :-     bomb(red).
yellow   :-     bomb(yellow).
blue     :-     bomb(blue).
seq      :-	green, red.
par      :-	green & red.
lpar     :-	green &   red &   yellow &   blue.
at(X)    :-	green @ X .
thr      :-	green &&, red.
lthr     :-	green &&, red &&, yellow &&, blue.

% Code: 
bomb(BId) :- 
	create_server(BId,PortId),
	canvas_init(PortId),
	pause(1),
	do_bomb(BId,PortId),
	pause(1),
	close_tcltk_server(PortId).

canvas_init(PortId) :-
	port_send(PortId,"canvas .c1"),
	port_send(PortId,"pack .c1 -fill both -expand yes"),
	port_send(PortId,".c1 configure -background bisque").

do_bomb(C,PortId) :-
	name(C,Cs),
	create_bomb(PortId,Cs),
	slide_bomb(200,PortId,Cs).

create_bomb(PortId,BId) :-
	append(".c1 create bitmap 35 40 -bitmap @bomb.xbm -foreground ", 
               BId, T1),
        append(T1, " -background bisque -tag ", T2), 
        append(T2, BId, Command),
	port_send(PortId, Command),
	worker(W),
	name(W,Ws),
	current_host(H),
	name(H,Hs),
	append(".c1 create text 40 65 -text ", Ws, T3),
	append(T3, "@", T4),
	append(T4, Hs, T5),
	append(T5, " -tag ", T6),
	append(T6, BId, Command1),
	port_send(PortId, Command1).

slide_bomb(0,_,_).
slide_bomb(N,PortId,BId) :-
	N > 0,
	move_bomb(PortId,BId," 1 ",2000),
	N1 is N-1,
	slide_bomb(N1,PortId,BId).

move_bomb(PortId,BId,Inc,Loop) :-
	append(".c1 move ", BId, T1),
	append(T1, Inc, T2),
	append(T2, Inc, Command),
	bomb_loop(Loop),
	port_send(PortId,Command).

create_server(red,PortId) :-
	!,
	create_tcltk_server(PortId,"280x280-0-30").
create_server(blue,PortId) :-
	!,
	create_tcltk_server(PortId,"280x280+0+0").
create_server(yellow,PortId) :-
	!,
	create_tcltk_server(PortId,"280x280+0-30").
create_server(_,PortId) :- 
	create_tcltk_server(PortId,"280x280-0+0").

append([],X,X).
append([X|Y],Z,[X|W]) :- append(Y,Z,W).

% Simple delay loop
bomb_loop(0).
bomb_loop(N) :-
	N > 0,
	N1 is N-1,
	bomb_loop(N1).


