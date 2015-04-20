% Example: Distributed Communication

:- module(dist_comm,_,ciao(h)).

:- use_module('threads.cl').

nosyn :- bomb(green) &&, bomb(red).
syn :-	sync_bomb(green, [ok|X]) &&, sync_bomb(red, X).

sync_bomb(BId,X) :- 
	sync_create_server(BId,PortId),
	sync_canvas_init(PortId),
	pause(1),
	sync_do_bomb(BId,PortId,X),
	pause(1),
	close_tcltk_server(PortId).

sync_canvas_init(PortId) :-
	port_send(PortId,"canvas .c1"),
	port_send(PortId,"pack .c1 -fill both -expand yes"),
	port_send(PortId,".c1 configure -background bisque").

sync_do_bomb(C,PortId,X) :-
	name(C,Cs),
	sync_create_bomb(PortId,Cs),
	sync_slide_bomb(100,PortId,Cs,X).

sync_create_bomb(PortId,BId) :-
	append(".c1 create bitmap 40 40 -bitmap @bomb.xbm -foreground ", 
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

sync_slide_bomb(N,PortId,BId,X) :-
	ask(nonvar(X)),
	X = [_|T],
	slide_bomb_body(N,PortId,BId,T).

slide_bomb_body(0,_,_,T) :-
	T = [ok|_].
slide_bomb_body(N,PortId,BId,T) :-
	N > 0,
	sync_move_bomb(PortId,BId," 2 "),
	T = [ok|NT],
	N1 is N-1,
	sync_slide_bomb(N1,PortId,BId,NT).

sync_move_bomb(PortId,BId,Inc) :-
	append(".c1 move ", BId, T1),
	append(T1, Inc, T2),
	append(T2, Inc, Command),
	port_send(PortId,Command).

sync_create_server(red,PortId) :-
	!,
	create_tcltk_server(PortId,"280x280-0-30").
sync_create_server(blue,PortId) :-
	!,
	create_tcltk_server(PortId,"280x280+0+0").
sync_create_server(yellow,PortId) :-
	!,
	create_tcltk_server(PortId,"280x280+0-30").
sync_create_server(_,PortId) :- 
	create_tcltk_server(PortId,"280x280-0+0").

append([],X,X).
append([X|Y],Z,[X|W]) :- append(Y,Z,W).

