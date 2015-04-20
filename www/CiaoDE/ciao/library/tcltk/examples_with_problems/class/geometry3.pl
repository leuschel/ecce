:- module(geometry3,[test/0],[objects]).

:- use_module(library(system),[pause/1]).
:- use_module(library(concurrency)).

:- use_class(library('tcltk/examples_with_problems/class/oval_class')).
:- use_class(library('tcltk/examples_with_problems/class/canvas_class')).
:- use_class(library('tcltk/examples_with_problems/class/poly_class')).
:- use_class(library('tcltk/examples_with_problems/class/arc_class')).

:- left_eye  instance_of  oval_class((47,49),13,23).
:- right_eye instance_of  oval_class((125,49),13,23).
:- nose      instance_of  poly_class([(77,81),(79,77),(84,81),(80,67)]).
:- mouth     instance_of  arc_class((85,85),119,119).
:- face      instance_of  oval_class((85,85),160,160).
:- face1     instance_of  canvas_class(
        [
            oval_class(face),
            oval_class(left_eye),
            oval_class(right_eye),
            poly_class(nose),
            arc_class(mouth)
        ]).

test:-
        left_eye:set_border_width(3),
        right_eye:set_border_width(3),
        left_eye:set_bg_color(blue),
        right_eye:set_bg_color(blue),

        face:set_border_width(2),
        face:set_bg_color(gray),

        mouth:set_border_width(3),
        mouth:set_angle_start(225),
        mouth:set_style('arc'),
        
        face1:show,
        hit_enter_delete('Eyes'),

        destroy oval_class(left_eye),
        destroy oval_class(right_eye),
        hit_enter_delete('Nose and Mouth'),
        destroy poly_class(nose),
        destroy arc_class(mouth),
        hit_enter_delete('Face'),
        destroy oval_class(face),
        hit_enter,

        destroy canvas_class(face1).
        

test :-
%       Delay = 10,

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

        Face2 new canvas_class([Face,Left_eye,Right_eye,Nose,Mouth]),
        Face2:show,

%       eng_call(move(25,Left_eye, down,2,Delay),create,create),
%       eng_call(move(25,Right_eye,down,2,Delay),create,create),

        hit_enter_delete('Eyes'),

        destroy Left_eye,
        destroy Right_eye,
        hit_enter_delete('Nose and Mouth'),
        destroy Nose,
        destroy Mouth,
        hit_enter_delete('Face'),
        destroy Face,
        hit_enter,

        destroy Face1,
        destroy Face2.

hit_enter :-
        display('Hit ENTER to continue...'),
        nl,
        get_code(_).

hit_enter_delete(X) :-
        display('Hit ENTER to delete '),
        display(X),
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
