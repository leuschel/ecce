:- module(geometry2,[test/0],[objects]).

:- use_class(library('tcltk/examples_with_problems/class/oval_class')).
:- use_class(library('tcltk/examples_with_problems/class/canvas_class')).
:- use_class(library('tcltk/examples_with_problems/class/poly_class')).
:- use_class(library('tcltk/examples_with_problems/class/arc_class')).

:- use_module(library(system),[pause/1]).
:- use_module(library(concurrency)).

test :-
%        Delay = 10,

        Left_eye new oval_class((47,49),13,23),
        Right_eye new oval_class((125,49),13,23),

        Left_eye:set_border_width(3),
        Right_eye:set_border_width(3),
        Left_eye:set_bg_color(blue),
        Right_eye:set_bg_color(blue),

        Nose new poly_class([(77,81),(79,77),(84,81),(80,67)]),

        Face new oval_class((85,85),160,160),

        Face:set_border_width(2),
        Face:set_bg_color(gray),

        Mouth new arc_class((85,85),119,119),

        Mouth:set_border_width(3),
        Mouth:set_angle_start(225),
        Mouth:set_style('arc'),

        Face1 new canvas_class([Face,Left_eye,Right_eye,Nose,Mouth]),
        Face1:show,

%        Face2 new canvas_class([Face,Left_eye,Right_eye,Nose,Mouth]),
%        Face2:show,

%        eng_call(move(4,Left_eye, down,2,Delay),create,create),
%        eng_call(move(4,Right_eye,down,2,Delay),create,create),

        hit_enter,

        destroy Left_eye,
        destroy Right_eye,
        destroy Face,
        destroy Nose,
        destroy Mouth,
        destroy Face1.
%        destroy Face2.

hit_enter :-
        display('Hit ENTER to continue...'),
        nl,
        get_code(_).


move(0,_O,_Direction,_Increment,_Delay).
move(Total,O,Direction,Increment,Delay) :-
        Total > 0,
        MyDelay is Delay * 10000,
        my_pause(MyDelay),
        O:get_center(X,Y),
        compute_move(Direction,Increment,X,Y,NX,NY),
        O:set_center(NX,NY),
        NTotal = Total-Increment,
        move(NTotal,O,Direction,Increment,Delay).
        
compute_move(right,Amount,X,Y,NX,Y) :- NX is X+Amount.
compute_move(left, Amount,X,Y,NX,Y) :- NX is X-Amount.
compute_move(up,   Amount,X,Y,X,NY) :- NY is Y-Amount.
compute_move(down, Amount,X,Y,X,NY) :- NY is Y+Amount.

my_pause(0).
my_pause(Delay) :- 
        Delay > 0,
        NDelay is Delay - 1,
        my_pause(NDelay).
