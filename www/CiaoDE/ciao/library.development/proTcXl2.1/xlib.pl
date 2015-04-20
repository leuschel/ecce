%
% ECLiPSe version of the Prolog Tk interface
%
% Author: Micha Meier
% Date:   September 93
%

%
% sccsid("@(#)xlib.pl	1.5          96/03/02").
% sccscr("@(#)  Copyright 1996 Micha Meier ").
%

 %% Module et.al. interface deleted

 %% :- module_interface(xlib).
 %% 
 %% :- tool(xlib_define_macros/0).
 %% 
 %% :- export
 %% 	inside/4,
 %% 	tr_xlib/2,
 %% 	xlib_arc/7,
 %% 	xlib_background/2,
 %% 	xlib_change_gc/3,
 %% 	xlib_clear/5,
 %% 	xlib_color/3,
 %% 	xlib_define_macros/0,
 %% 	xlib_fill_arc/7,
 %% 	xlib_fill_rectangle/5,
 %% 	xlib_flush/1,
 %% 	xlib_font/2,
 %% 	xlib_foreground/2,
 %% 	xlib_function/2,
 %% 	xlib_id/4,
 %% 	xlib_image_string/4,
 %% 	xlib_init/2,
 %% 	xlib_line/5,
 %% 	xlib_line_attributes/5,
 %% 	xlib_pixmap/3,
 %% 	xlib_polygon/9,
 %% 	xlib_query_pointer/3,
 %% 	xlib_rectangle/5,
 %% 	xlib_string/4,
 %% 	xlib_tkwin/2,
 %% 	xlib_warp_pointer/3.

xlib_define_macros.		% dummy for the next query

:- xlib_define_macros.

 %% :- begin_module(xlib).
 %% 
 %% :- import define_macro_/4 from sepia_kernel.
 %% 
 %% :- [tr_xlib].
 %% 
 %% :- xlib_define_macros.
 %% 
 %% :- make_array(xlib_gcvalues(30), integer).
 %% 
 %% :- external(inside/4, p_inside).
 %% :- external(xlib_init/2, p_xlib_init).
 %% :- external(xlib_id/4, p_xlib_id).
 %% :- external(xlib_color/3, p_xlib_color).
 %% :- external(xlib_font/2, p_xlib_font).
 %% :- external(xlib_pixmap/3, p_xlib_pixmap).
 %% :- external(xlib_polygon/9, p_xlib_polygon).
 %% :- external(xlib_query_pointer/3, p_xlib_query_pointer).
 %% :- external(xlib_tkwin/2, p_xlib_tkwin).

foreign_file('xlib.o',
        [p_inside,
         p_xlib_init,
         p_xlib_id,
         p_xlib_color,
         p_xlib_font,
         p_xlib_pixmap,
         p_xlib_polygon,
         p_xlib_query_pointer,
         p_xlib_tkwin]).

 %% Arguments temptative.
 %% How should we treat the lists? 

foreign(p_inside, c, inside()).
foreign(p_xlib_init ,c , xlib_init()).
foreign(p_xlib_id ,c , xlib_id()).
foreign(p_xlib_color ,c , xlib_color()).
foreign(p_xlib_font ,c , xlib_font()).
foreign(p_xlib_pixmap ,c , xlib_pixmap()).
foreign(p_xlib_polygon ,c , xlib_polygon()).
foreign(p_xlib_query_pointer ,c , xlib_query_pointer()).
foreign(p_xlib_tkwin ,c , xlib_tkwin()).



%
% The most frequently used Xlib operations
%
xlib_flush(XID) :-
    xlib_id(XID, Display, _, _),
    call_c('XFlush'(Display), _).       % How is call_c defined?

xlib_foreground(XID, Color) :-
    xlib_color(XID, Color, XC),
    xlib_id(XID, Display, _, GC),
    call_c('XSetForeground'(Display, GC, XC), _).
    
xlib_background(XID, Color) :-
    xlib_color(XID, Color, XC),
    xlib_id(XID, Display, _, GC),
    call_c('XSetBackground'(Display, GC, XC), _).
    
xlib_function(XID, Function) :-
    xlib_id(XID, Display, _, GC),
    call_c('XSetFunction'(Display, GC, Function), _).
    
xlib_change_gc(XID, Name, Value) :-
    xlib_id(XID, Display, _, GC),
    Index0 is fix(ln(Name)/ln(2.0) + 0.1),
    % fix the arc_mode irregularity
    (Index0 < 10 ->
        Index = Index0
    ;
    Index0 = 22 ->
	Index = 10
    ;
        Index is Index0 + 1
    ),
    setval(xlib_gcvalues(Index), Value),
    call_c('XChangeGC'(Display, GC, Name, xlib_gcvalues/1), _).
    
xlib_image_string(XID, Thing, X, Y) :-
    xlib_id(XID, Display, Window, GC),
    id_length(Thing, String, Len),
    call_c('XDrawImageString'(Display, Window, GC, X, Y, String, Len), _).

xlib_string(XID, Thing, X, Y) :-
    xlib_id(XID, Display, Window, GC),
    id_length(Thing, String, Len),
    call_c('XDrawString'(Display, Window, GC, X, Y, String, Len), _).

xlib_arc(XID, X, Y, Width, Height, Angle1, Angle2) :-
    xlib_id(XID, Display, Win, GC),
    IA1 is floor(Angle1*64),
    IA2 is floor(Angle2*64),
    call_c('XDrawArc'(Display, Win, GC, X, Y, Width, Height, IA1, IA2), _).

xlib_fill_arc(XID, X, Y, Width, Height, Angle1, Angle2) :-
    xlib_id(XID, Display, Win, GC),
    IA1 is floor(Angle1*64),
    IA2 is floor(Angle2*64),
    call_c('XFillArc'(Display, Win, GC, X, Y, Width, Height, IA1, IA2), _).

xlib_clear(XID, X, Y, Width, Height) :-
    xlib_id(XID, Display, Win, _),
    call_c('XClearArea'(Display, Win, X, Y, Width, Height, 0), _).

xlib_line(XID, X1, Y1, X2, Y2) :-
    xlib_id(XID, Display, Win, GC),
    call_c('XDrawLine'(Display, Win, GC, X1, Y1, X2, Y2), _).

xlib_line_attributes(XID, Width, Style, Cap, Join) :-
    xlib_id(XID, Display, _, GC),
    call_c('XSetLineAttributes'(Display, GC, Width, Style, Cap, Join), _).

xlib_rectangle(XID, X, Y, Width, Height) :-
    xlib_id(XID, Display, Win, GC),
    call_c('XDrawRectangle'(Display, Win, GC, X, Y, Width, Height), _).

xlib_fill_rectangle(XID, X, Y, Width, Height) :-
    xlib_id(XID, Display, Win, GC),
    call_c('XFillRectangle'(Display, Win, GC, X, Y, Width, Height), _).

xlib_warp_pointer(XID, X, Y) :-
    xlib_id(XID, Display, Win, _),
    call_c('XWarpPointer'(Display, xNone, Win, 0, 0, 0, 0, X, Y), _).

xlib_stipple(XlibID, Bitmap) :-
    tk_id(XlibID, TkWin, Interp),
    xlib_id(XlibID, Display, _, GC),
    (Bitmap = "" ->
	call_c('XSetFillStyle'(Display, GC, xFillSolid))
    ;
	call_c('Tk_GetUid'(Bitmap), BitmapUid),
	call_c('Tk_GetBitmap'(Interp, TkWin, BitmapUid), Pixmap),
	call_c('XSetStipple'(Display, GC, Pixmap), _),
	call_c('XSetFillStyle'(Display, GC, xFillStippled), _)
    ).

id_length(A, A, L) :-
    atom(A),
    atom_length(A, L).
id_length(S, S, L) :-
    string(S),
    string_length(S, L).
id_length(N, S, L) :-
    number(N),
    number_string(N, S),
    id_length(S, _, L).
