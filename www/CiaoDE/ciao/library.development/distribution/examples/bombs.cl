% Example: concurrency and parallelism operators

:- module(threads,_,ciao(h)).

% Some sample queries.

seq      :-	bomb(green) ,   bomb(red).
par      :-	bomb(green) &   bomb(red).
lpar     :-	bomb(green) &   bomb(red) & bomb(yellow) & bomb(blue).
thr      :-	bomb(green) &&, bomb(red).
lthr     :-	bomb(green) &&, bomb(red) &&, bomb(yellow) &&, bomb(blue).
% explicit :-	bomb(green) &@ alba, bomb(red).

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
	port_send(PortId,"pack .c1 -fill both -expand yes").

do_bomb(C,PortId) :-
	name(C,Cs),
	create_bomb(PortId,Cs),
	slide_bomb(200,PortId,Cs).

create_bomb(PortId,BId) :-
	string_append([
            ".c1 create bitmap 35 40 -bitmap @bomb.xbm -foreground ", 
            BId, " -background bisque -tag ", BId], Command),
	port_send(PortId, Command),
	worker(W),
	current_host(H),
	string_append([
            ".c1 create text 40 65 -text ", W, "@", H, " -tag ", BId],
                      Command1),
	port_send(PortId, Command1).

slide_bomb(0,_,_).
slide_bomb(N,PortId,BId) :-
	N > 0,
	move_bomb(PortId,BId," 1 ",2000),
	N1 is N-1,
	slide_bomb(N1,PortId,BId).

move_bomb(PortId,BId,Inc,Loop) :-
	string_append([".c1 move ", BId, Inc, Inc], Command),
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

% Simple delay loop
bomb_loop(0).
bomb_loop(N) :-
	N > 0,
	N1 is N-1,
	bomb_loop(N1).



append([], X, X).
append([X|Xs], Y, [X|Zs]) :-
	append(Xs,Y,Zs).
