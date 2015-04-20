%%------------------------------------------------------------------------
%%
%% PROLOG INTERFACE TO FLY
%%
%% AUTHOR : Angel Fernandez Pineda
%% DATE   : March 1999
%%
%%------------------------------------------------------------------------

:- class(fly).

:- public 
	[
	    fly/1,
	    fly/3,
	    update_gif/0,
	    clear/0,
%	    destructor/0,
	    destination/1,
	    height/1,
	    width/1,
	    transparent/1,
	    color_map/2,
	    brush/1,
	    pixel/3,
	    line/5,
	    dashed_line/5,
	    fill/3,
	    flood_fill/4,
	    circle/3,
	    solid_circle/3,
	    ellipse/3,
	    arc/4,
	    polygon/2,
	    solid_polygon/2,
	    rectangle/5,
	    box/5
	].

%%------------------------------------------------------------------------


%%------------------------------------------------------------------------

:- use_module(library(system)).
:- use_module(library(write)).

%%------------------------------------------------------------------------

:- data dest/1.  % Destination file.
:- data size/2.  % Destination size.
:- data cmd/1.   % Stream to fly interpreter.

%%------------------------------------------------------------------------
%% CREATE FLY INTERPRETER
%%------------------------------------------------------------------------

start_fly_interpreter :-
	popen('fly -q',write,Strm),
	set_fact(cmd(Strm)).

%%------------------------------------------------------------------------
%% perform simple fly command (low level primitive)
%%------------------------------------------------------------------------

command(Command,Args,Strm) :-
	write(Strm,Command),
	write(Strm,' '),
	!,
	command_args(Args,Strm).

command_args([],Strm) :-
	nl(Strm),
	nl(Strm),
	flush_output(Strm),
	!.

command_args([Atom],Strm) :-
	write(Strm,Atom),
	nl(Strm),
	nl(Strm),
	flush_output(Strm),
	!.

command_args([Atom|Na],Strm) :-
	write(Strm,Atom),
	write(Strm,','),
	!,
	command_args(Na,Strm).

%%------------------------------------------------------------------------
%% VALIDATE PARAMETER
%%------------------------------------------------------------------------

validate_point(X,Y) :-
	integer(X),
	integer(Y).

validate_rgb(R,G,B) :-
	integer(R),
	integer(G),
	integer(B),
	R >= 0,
	G >= 0,
	B >= 0,
	R =< 255,
	G =< 255,
	B =< 255.

%%------------------------------------------------------------------------
%% DESTROY FLY INTERPRETER
%%------------------------------------------------------------------------

destructor :-
	cmd(Strm),
	command(end,[],Strm),
	close(Strm),
	retractall_fact(dest(_)),
	retractall_fact(cmd(_)).

%%------------------------------------------------------------------------
%% SET DESTINATION FILE
%%------------------------------------------------------------------------

fly(X,Y,F) :-
	fly_aux(X,Y,F).

fly(F) :-
	fly_aux(F).

fly_aux(Xsize,Ysize,FileName) :-
	integer(Xsize),
	integer(Ysize),
	atom(FileName),
	Xsize > 0,
	Ysize > 0,
	\+ cmd(_),
	!,
	start_fly_interpreter,
	cmd(Strm),
	set_fact(dest(FileName)),
	set_fact(size(Xsize,Ysize)),
	command(new,[],Strm),
	command(size,[Xsize,Ysize],Strm),
	command(name,[FileName],Strm),
	fill(1,1,white).

fly_aux(Xsize,Ysize,FileName) :-
	integer(Xsize),
	integer(Ysize),
	atom(FileName),
	!,
	display('Fly interpreter already running'),
	nl.

fly_aux(Xsize,Ysize,_) :-
	integer(Xsize),
	integer(Ysize),
	!,
	display('GIF path must be an atom'),
	nl.

fly_aux(_,_,_) :-
	!,
	display('invalid X/Y size parameter'),
	nl.

fly_aux(FileName) :-
	atom(FileName),
	\+ cmd(_),
	!,
	start_fly_interpreter,
	cmd(Strm),
	set_fact(dest(FileName)),
	command(existing,[FileName],Strm),
	command(name,[FileName],Strm).

fly_aux(FileName) :-
	atom(FileName),
	!,
	display('Fly interpreter already running'),
	nl.

fly_aux(_) :-
	!,
	display('GIF path must be an atom'),
	nl.

%%------------------------------------------------------------------------
%% RETRIEVE GIF PARAMETERS
%%------------------------------------------------------------------------

destination(Dest) :-
	dest(Dest).

height(H) :-
	size(_,H).

width(W) :-
	size(W,_).

%%------------------------------------------------------------------------
%% UPDATE
%%------------------------------------------------------------------------

update_gif :-
	dest(FileName),
	destructor,
	fly(FileName).

%%------------------------------------------------------------------------
%% CLEAR
%%------------------------------------------------------------------------

clear :-
	cmd(Strm),
	dest(FileName),
	!,
	command(end,[],Strm),
	command('new',[],Strm),
	command('name',[FileName],Strm).

%%------------------------------------------------------------------------
%% FLY COLOR AND PALETTE COMMANDS
%%------------------------------------------------------------------------

% set transparent color

transparent(Color) :-
	cmd(Strm),
	rgb_color(Color,rgb(R,G,B)),
	!,
	command(transparent,[R,G,B],Strm).

% map color

color_map(Color1,Color2) :-
	cmd(Strm),
	rgb_color(Color1,rgb(R1,G1,B1)),
	rgb_color(Color2,rgb(R2,G2,B2)),
	!,
	command(colourchange,[R1,G1,B1,R2,G2,B2],Strm).

% set brush

brush(none) :-
	cmd(Strm),
	command(killbrush,[],Strm).
	
brush(File) :-
	cmd(Strm),
	atom(File),
	!,
	command(setbrush,[File],Strm).

%%------------------------------------------------------------------------
%% FLY DRAWING COMMANDS
%%------------------------------------------------------------------------

% write simple pixel

pixel(X,Y,Color) :-
	cmd(Strm),
	rgb_color(Color,rgb(R,G,B)),
	validate_point(X,Y),
	!,
	command(setpixel,[X,Y,R,G,B],Strm).

% write line with current brush and style

line(X1,Y1,X2,Y2,Color) :-
	cmd(Strm),
	validate_point(X1,Y1),
	validate_point(X2,Y2),
	rgb_color(Color,rgb(R,G,B)),
	!,
	command(line,[X1,Y1,X2,Y2,R,G,B],Strm).

% write dashed line with current brush and style

dashed_line(X1,Y1,X2,Y2,Color) :-
	cmd(Strm),
	validate_point(X1,Y1),
	validate_point(X2,Y2),
	rgb_color(Color,rgb(R,G,B)),
	!,
	command(dline,[X1,Y1,X2,Y2,R,G,B],Strm).

% draw rectangle using solid line

rectangle(X1,Y1,X2,Y2,Color) :-
	cmd(Strm),
	validate_point(X1,Y1),
	validate_point(X2,Y2),
	rgb_color(Color,rgb(R,G,B)),
	!,
	command(rect,[X1,Y1,X2,Y2,R,G,B],Strm).

% draw a solid box using solid line

box(X1,Y1,X2,Y2,Color) :-
	cmd(Strm),
	validate_point(X1,Y1),
	validate_point(X2,Y2),
	rgb_color(Color,rgb(R,G,B)),
	!,
	command(frect,[X1,Y1,X2,Y2,R,G,B],Strm).

% fill region using solid color

fill(X,Y,Color) :-
	cmd(Strm),
	validate_point(X,Y),
	rgb_color(Color,rgb(R,G,B)),
	!,
	command(fill,[X,Y,R,G,B],Strm).

% draw polygon

check_point_list([point(X,Y)|Np],[X,Y|Na]) :-
	validate_point(X,Y),
	!,
	check_point_list(Np,Na).

polygon(PointList,Color) :-
	cmd(Strm),
	rgb_color(Color,rgb(R,G,B)),
	check_point_list(PointList,ArgList),
	!,
	command(poly,[R,G,B|ArgList],Strm).

% draw solid polygon

solid_polygon(PointList,Color) :-
	cmd(Strm),
	rgb_color(Color,rgb(R,G,B)),
	check_point_list(PointList,ArgList),
	!,
	command(fpoly,[R,G,B|ArgList],Strm).

% draw an arc

arc(center(X,Y),size(W,H),angle(S,F),Color) :-
	cmd(Strm),
	validate_point(X,Y),
	rgb_color(Color,rgb(R,G,B)),
	integer(W),
	integer(H),
	integer(S),
	integer(F),
	!,
	command(arc,[X,Y,W,H,S,F,R,G,B],Strm).

% draw ellipse

ellipse(center(X,Y),size(W,H),Color) :-
	cmd(Strm),
	rgb_color(Color,rgb(R,G,B)),
	validate_point(X,Y),
	integer(W),
	integer(H),
	!,
	command(ellipse,[X,Y,W,H,R,G,B],Strm).


% draw a circle

circle(center(X,Y),D,Color) :-
	cmd(Strm),
	rgb_color(Color,rgb(R,G,B)),
	validate_point(X,Y),
	integer(D),
	!,
	command(circle,[X,Y,D,R,G,B],Strm).

% draw a solid circle

solid_circle(center(X,Y),D,Color) :-
	cmd(Strm),
	rgb_color(Color,rgb(R,G,B)),
	validate_point(X,Y),
	integer(D),
	!,
	command(fcircle,[X,Y,D,R,G,B],Strm).

% flood fill to border

flood_fill(X,Y,BColor,Color) :-
	cmd(Strm),
	rgb_color(Color,rgb(R,G,B)),
	rgb_color(BColor,rgb(BR,BG,BB)),
	validate_point(X,Y),
	!,
	command(filltoborder,[X,Y,BR,BG,BB,R,G,B],Strm).

%%------------------------------------------------------------------------
%% COPY FROM OTHER GIF
%%------------------------------------------------------------------------

%%------------------------------------------------------------------------
%% SOME COLOR MAPPINGS
%%------------------------------------------------------------------------

rgb_color(rgb(R,G,B),rgb(R,G,B)) :- 
	validate_rgb(R,G,B),
	!.

rgb_color(snow,rgb(255,250,250)).
rgb_color(red,rgb(255,0,0)).
rgb_color(green,rgb(0,255,0)).
rgb_color(blue,rgb(0,0,255)).
rgb_color(black,rgb(0,0,0)).
rgb_color(white,rgb(255,255,255)).
